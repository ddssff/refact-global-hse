{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, FlexibleInstances, PackageImports #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}

module Refactor.ModuleInfo
    ( ModuleInfo(..)
    , addScope
    , unScope
    , exactPrint'
    , putModuleDecls
    , putNewModules
    , fullPathOfModuleInfo
    , getTopSymbols
    , getTopDeclSymbols'
    , withDecomposedModule
    , decomposeModule
    , adjustSpan
    , adjustSpanTests
    , scanModule
    ) where

import Control.Monad (foldM, when)
import Control.Monad.RWS (execRWS, get, put, RWS, tell)
import Control.Monad.State (State)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.Graph.Inductive (Gr, NodeMap, components)
import Data.Maybe (mapMaybe)
import Data.Set as Set (empty, fromList, insert, intersection, map, member, notMember, null, Set, union)
import Debug.Trace
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.ExactPrint (exactPrint)
import Language.Haskell.Exts.Pretty ({-Pretty,-} prettyPrint)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax -- (Decl(TypeSig), Module(Module), ModuleHead(ModuleHead), Name)
import Language.Haskell.Names (annotate, {-NameInfo,-} resolve, Scoped(Scoped), Symbol(..))
import Language.Haskell.Names.GetBound (getBound)
import Language.Haskell.Names.GlobalSymbolTable as Global (lookupName, Table)
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols, moduleTable)
import Language.Haskell.Names.SyntaxUtils -- (dropAnn)
import Refactor.FGL
import Refactor.ModuleKey (moduleFullPath, ModuleKey{-, moduleName'-})
import Refactor.SrcSpan (SpanInfo(srcSpan), textOfSpan, HasSrcSpanInfo(srcSpanInfo))
import Refactor.Utils (con, EZPrint(ezPrint), gFind)
import Test.HUnit (Test(TestList, TestCase), assertEqual, runTestTT)

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

deriving instance Ord Comment

data ModuleInfo l =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: Module l
               , _moduleComments :: [Comment]
               , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               , _moduleGlobals :: Global.Table
               } deriving (Data, Typeable, Functor, Show)

instance EZPrint (ModuleInfo l) where
    ezPrint (ModuleInfo {_module = Module _ (Just (ModuleHead _ n _ _)) _ _ _}) = prettyPrint n
    ezPrint (ModuleInfo {_module = Module _ Nothing _ _ _}) = "Main"
    ezPrint (ModuleInfo {_module = _}) = error "ezPrint: unexpected module"

exactPrint' :: ModuleInfo SrcSpanInfo -> String
exactPrint' i = exactPrint (_module i) (_moduleComments i)

addScope :: [ModuleInfo SrcSpanInfo] -> [ModuleInfo (Scoped SrcSpanInfo)]
addScope mods =
    fmap (\m -> m {_module = annotate env (_module m),
                   _moduleGlobals = moduleTable (importTable env (_module m)) (_module m)}) mods
    where env = resolve (fmap _module mods) mempty

unScope :: Scoped a -> a
unScope (Scoped _ l) = l

fullPathOfModuleInfo :: ModuleInfo l -> FilePath
fullPathOfModuleInfo = moduleFullPath . _moduleKey

-- | A top symbol is either one returned by topDeclSymbols or a reference to
-- a one of the top decl symbols in this module, as in a type signature.
--
--     λ> [i] <- loadModule def (ModuleFilePath (Just "src") "Refactor/Utils.hs")
--     λ> getTopSymbols i
--     fromList [Value {symbolModule = ModuleName () "Refactor.Utils", symbolName = Ident () "cartesianProduct"}, ...
getTopSymbols :: (Data l, Eq l) => ModuleInfo l -> Set Symbol
getTopSymbols i@(ModuleInfo {_module = Module _ _ _ _ ds}) =
    Set.fromList $ mconcat $ fmap (\d -> getTopDeclSymbols (_moduleGlobals i) (getModuleName (_module i)) d) ds
getTopSymbols _ = Set.empty

getTopDeclSymbols' :: (Data l, Eq l) => ModuleInfo l -> Decl l -> Set Symbol
getTopDeclSymbols' i d = Set.fromList $ getTopDeclSymbols (_moduleGlobals i) (getModuleName (_module i)) d

instance (SrcInfo l, Typeable l, Data l, Eq l) => EZPrint (ModuleInfo l, Decl l) where
    ezPrint (_, InstDecl _ _ r _) = ezPrint r
    ezPrint (_, SpliceDecl _ e) = ezPrint e
    ezPrint (i, d) =
        case gFind (getTopDeclSymbols' i d) :: [Name ()] of
          [] -> case (getBound (_moduleGlobals i) d) of
                  [] -> "(con=" ++ con d ++ ") " ++ show (getTopDeclSymbols' i d)
                  xs -> ezPrint xs
          ns -> ezPrint ns

{-
-- | Given a module name and a function that specifies where a module's
-- declarations should be moved to, compute a new set of modules
-- reflecting that transformation.
partitionModule :: ((ModuleName l, Decl l) -> ModuleName l) -> [ModuleInfo l] -> ([ModuleInfo l]
partitionModule f ms =
-}

newtype Decs l = Decs {unDecs :: [Decl l]} deriving (Data, Eq, Ord, Show, Functor)

putNewModules :: [ModuleInfo SrcSpanInfo] -> IO ()
putNewModules ms =
    mapM_ (\(m, n) -> writeFile ("Tmp" ++ show n ++ ".hs") (exactPrint (_module m) (_moduleComments m))) (zip ms [1..])

-- Filter some declarations out of a module.  To do this we build a
-- graph of the "uses" relation on the module's declaration.  The
-- result is the declarations reachable in the graph and the graph's
-- complement.
--
--     λ> [i] <- loadModules def [ModuleFilePath (Just "src") "Refactor/Graph.hs"]
--     λ> let s = Value {symbolModule = ModuleName () "Refactor.Graph", symbolName = Language.Haskell.Exts.Ident () "findModuleByKeyUnsafe"}
--     λ> putNewModules (fmap unScope i) (fmap (fmap unScope) (decomposeModule (\d -> member s (getTopDeclSymbols' i d)) i))
decomposeModule :: ModuleInfo (Scoped SrcSpanInfo) -> [ModuleInfo (Scoped SrcSpanInfo)]
decomposeModule i = withDecomposedModule putModuleDecls i

withDecomposedModule ::
    forall r.
       (ModuleInfo (Scoped SrcSpanInfo)
        -> (Decl (Scoped SrcSpanInfo) -> Bool)
        -> (ExportSpec (Scoped SrcSpanInfo) -> Bool)
        -> (Comment -> Bool) -> r)
    -> ModuleInfo (Scoped SrcSpanInfo) -> [r]
withDecomposedModule f i@(ModuleInfo {_module = Module _l _h _ps _is _ds, _moduleComments = cs}) =
    fmap (uncurry3 (f i)) (zip3 (fmap (flip Set.member) selectedDecls) (fmap (flip Set.member) selectedExports) (fmap (flip Set.member) selectedComments))
    where
      selectedDecls :: [Set (Decl (Scoped SrcSpanInfo))]
      selectedDecls = partitionDecls i
      selectedExports:: [Set (ExportSpec (Scoped SrcSpanInfo))]
      selectedExports = fmap (exportsToKeep i) selectedDecls
      selectedComments = fmap (uncurry (filterComments (fmap (srcInfoSpan . unScope) i)))
                           (zip (fmap (Set.map (fmap (srcInfoSpan . unScope))) selectedDecls)
                                (fmap (Set.map (fmap (srcInfoSpan . unScope))) selectedExports))
withDecomposedModule f i@(ModuleInfo {_module = m}) = [f i (const True) (const True) (const True)]

-- | We now have sets describing which declarations to keep and which
-- export specs to keep.  This function partitions the comment list.
-- (Comments associated with the header outside the export list are
-- duplicated in all modules.)
filterComments ::
       ModuleInfo SrcSpan
    -> Set (Decl SrcSpan)
    -> Set (ExportSpec SrcSpan)
    -> Set Comment
filterComments i@(ModuleInfo {_module = Module l h ps is ds, _moduleComments = cs}) selectedDecls selectedExports =
    -- In the export list we remove anything before a removed
    -- ExportSpec, and if the final ExportSpec is removed we remove
    -- anything after it until the end of the ExportSpecList.
    Set.fromList (prescan1 exportStatus cs [])
    --Set.union (selectedComments cs Set.empty) filterDeclComments
    where
      exportStatus :: [(ExportSpec SrcSpan, Bool)]
      exportStatus = case h of
                       Just (ModuleHead _ _ _ (Just (ExportSpecList _ es))) -> zip es (fmap (`Set.member` selectedExports) es)
                       _ -> []
      declStatus :: [(Decl SrcSpan, Bool)]
      declStatus = zip ds (fmap (`Set.member` selectedDecls) ds)

      prescan1 :: [(ExportSpec SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      prescan1 es@((e, _) : more) (c1 : c2@(Comment _ cspan _) : cs) r
        | srcSpanEnd cspan < srcSpanStart (ann e) = prescan1 es (c2 : cs) (c1 : r)
      prescan1 es cs r = scanExports es cs r

      scanExports :: [(ExportSpec SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      scanExports es@((e, False) : more) (c1@(Comment _ cspan _) : cs) r
        | srcSpanEnd cspan < srcSpanEnd (ann e) = scanExports es cs r
      scanExports es@((e, True) : more) (c1@(Comment _ cspan _) : cs) r
        | srcSpanEnd cspan < srcSpanEnd (ann e) = scanExports es cs (c1 : r)
      scanExports ((e, _) : more) (c1 : cs) r = scanExports more (c1 : cs) r
      scanExports [] cs r = prescan2 declStatus cs r
      scanExports _ [] r = prescan2 declStatus cs r

      -- Scan comments thru the one before the one before the first decl
      prescan2 :: [(Decl SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      prescan2 ds@((d, _) : more) (c1 : c2@(Comment _ cspan _) : cs) r
        | srcSpanEnd cspan < srcSpanStart (ann d) = prescan2 ds (c2 : cs) (c1 : r)
      prescan2 ds cs r = scanDecls ds cs r
      -- We have reached the comment that directly precedes the
      -- declaration section.
      scanDecls :: [(Decl SrcSpan, Bool)] -> [Comment] -> [Comment] -> [Comment]
      scanDecls ds@((d, False) : more) (c1@(Comment _ cspan _) : cs) r
        | srcSpanEnd cspan < srcSpanEnd (ann d) = scanDecls ds cs r
      scanDecls ds@((d, True) : more) (c1@(Comment _ cspan _) : cs) r
        | srcSpanEnd cspan < srcSpanEnd (ann d) = scanDecls ds cs (c1 : r)
      scanDecls ((d, _) : more) (c1 : cs) r = scanDecls more (c1 : cs) r
      scanDecls [] cs r = reverse r ++ cs
      scanDecls _ [] r = reverse r

-- | Scan the module elements in order and print it.
scanModule ::
  forall l. (HasSrcSpanInfo l, SpanInfo l)
  => ModuleInfo l
  -> (Decl l -> Bool)
  -> (ExportSpec l -> Bool)
  -> (Comment -> Bool)
  -> String
scanModule i@(ModuleInfo {_module = Module l h ps is ds, _moduleComments = cs}) selectedDecls selectedExports selectedComments =
  snd $ execRWS (pre >> scanPragmas >> scanHeader h >> scanImports >> scanDecls >> post) () (1, 1)
  where
    (SrcSpanInfo (SrcSpan _ msl msc mel mec) _) = _moduleSpan i
    pre = keep (msl, msc)
    scanPragmas = mapM_ (keepE . ann) ps
    scanHeader :: Maybe (ModuleHead l) -> RWS () String (Int, Int) ()
    scanHeader Nothing = return ()
    scanHeader (Just h@(ModuleHead _ n w me)) = do
      keepE (ann n)
      maybe (return ()) (keepE . ann) w
      maybe (return ()) scanExports me
      keepE (ann h)

    scanExports :: ExportSpecList l -> RWS () String (Int, Int) ()
    scanExports esl@(ExportSpecList _ es) = do
      let si = srcSpanInfo (ann esl)
      case srcInfoPoints si of
        [] -> keepV "es" (srcSpanStart (srcSpan (ann esl)))
        (p : ps) -> keepS p
      foldM scanExport True es
      keep {-"Y"-} (srcSpanEnd (srcSpan (ann esl)))
    scanExport :: Bool -> ExportSpec l -> RWS () String (Int, Int) Bool
    scanExport prev e =
        case selectedExports e of
          True -> do
            (if prev then keep else skip) (srcSpanStart (srcSpan (ann e)))
            keep (srcSpanEnd (srcSpan (ann e)))
            return True
          False -> do
            (if prev then keep else skip) (srcSpanStart (srcSpan (ann e)))
            skip {-"ex"-} (srcSpanEnd (srcSpan (ann e)))
            return False

    scanImports = mapM_ (keepE . ann) is
    scanDecls = foldM scanDecl True ds

    scanDecl :: Bool -> Decl l -> RWS () String (Int, Int) Bool
    scanDecl prev d =
        case selectedDecls d of
          True -> do
            (if prev then keep else skip) (srcSpanStart (srcSpan (ann d)))
            keep (srcSpanEnd (srcSpan (ann d)))
            return True
          False -> do
            (if prev then keep else skip) (srcSpanStart (srcSpan (ann d)))
            skip (srcSpanEnd (srcSpan (ann d)))
            return False
{-
        mapM_ (\d -> case selectedDecls d of
                       False -> skipV "d" (srcSpanEnd (srcSpan (ann d)))
                       True -> keepV "7" (srcSpanEnd (srcSpan (ann d)))) ds
-}
    post = keep (mel, mec)

    skip :: (Int, Int) -> RWS () String (Int, Int) ()
    skip = put

    skipV :: String -> (Int, Int) -> RWS () String (Int, Int) ()
    skipV label st = do
      here <- get
      when (here /= st) (tell ("[" ++ label ++ " - skipping from " ++ show here ++ " to " ++ show st ++ "]"))
      skip st

    keep :: (Int, Int) -> RWS () String (Int, Int) ()
    keep (el, ec) = do
      (l, c) <- get
      tell (textOfSpan (SrcSpan "" l c el ec) (_moduleText i))
      put (el, ec)

    keepV :: String -> (Int, Int) -> RWS () String (Int, Int) ()
    keepV n s = tell ("[" ++ n) >> keep s >> tell "]"

    keepS :: forall l. SpanInfo l => l -> RWS () String (Int, Int) ()
    keepS s = keep (srcSpanStart (srcSpan s))

    keepE :: forall l. SpanInfo l => l -> RWS () String (Int, Int) ()
    keepE s = keep (srcSpanEnd (srcSpan s))

    skip' :: (Int, Int) -> RWS () String (Int, Int) ()
    skip' pos =
        get >>= \pos0 ->
        if pos == pos0 then return () else tell "[xxx " >> keep pos >> tell "]"
    keep' :: forall l. SpanInfo  l => String -> l -> RWS () String (Int, Int) ()
    keep' n s = do
      skipV "k" (srcSpanStart (srcSpan s))
      tell ("[" ++ n)
      keep (srcSpanEnd (srcSpan s))
      tell "]"
    keep'' :: forall l. SpanInfo  l => l -> RWS () String (Int, Int) ()
    keep'' s = tell "[" >> keepS s >> keepE s >> tell "]"

adjustSpanTests :: Test
adjustSpanTests =
  TestList
  [ TestCase (assertEqual "test a1" (1, 2)  (adjustPos (SrcSpan "" 1 10 2 20) (1, 2)))
  , TestCase (assertEqual "test b1" (1, 10) (adjustPos (SrcSpan "" 2 10 3 20) (1, 10)))
  , TestCase (assertEqual "test c1" (1, 10) (adjustPos (SrcSpan "" 1 10 2 20) (2, 30)))
  , TestCase (assertEqual "test c1" (2, 1)  (adjustPos (SrcSpan "" 1 10 2 20) (3, 1)))
  , TestCase (assertEqual "test a2" (1, 5)  (adjustPos (SrcSpan "" 1 10 2 20) (1, 5)))
  , TestCase (assertEqual "test b2" (2, 10) (adjustPos (SrcSpan "" 2 10 3 20) (2, 40)))
  , TestCase (assertEqual "test c2" (1, 20) (adjustPos (SrcSpan "" 1 10 2 20) (2, 40)))
  , TestCase (assertEqual "test c2" (3, 20) (adjustPos (SrcSpan "" 1 10 2 20) (4, 20)))
  , TestCase (assertEqual "test a3" (SrcSpan "" 1 2 1 5)   (adjustSpan (SrcSpan "" 1 10 2 20) (SrcSpan "" 1 2 1 5)))
  , TestCase (assertEqual "test b3" (SrcSpan "" 1 10 2 10) (adjustSpan (SrcSpan "" 2 10 3 20) (SrcSpan "" 1 10 2 40)))
  , TestCase (assertEqual "test c3" (SrcSpan "" 1 10 1 20) (adjustSpan (SrcSpan "" 1 10 2 20) (SrcSpan "" 2 30 2 40)))
  , TestCase (assertEqual "test c3" (SrcSpan "" 2 1 3 20)  (adjustSpan (SrcSpan "" 1 10 2 20) (SrcSpan "" 3 1 4 20)))
  ]


adjustPos :: SrcSpan -> (Int, Int) -> (Int, Int)
adjustPos (SrcSpan f sl sc el ec) (l, c)
    | l < sl || (l == sl && c <= sc) = (l, c) -- unaffected
    | sl == el && l > sl = (l, c) -- Intra line change before l - unaffected
    -- Removed segement is within a single line
    | sl == el && l == sl && c > sc && c < ec = (l, sc)
    | sl == el && l == sl && c >= ec = (l, c - (ec - sc))
    -- Removed segment includes a newline
    | sl < el && l < el = (sl, sc)
    -- Remove segment ends before position
    | sl < el && l == el = let h = el - sl in (l - h, c - ec)
    | sl < el && l >= el = let h = el - sl in (l - h, c)
    | otherwise = error "adjustPos"

adjustSpan :: SrcSpan -> SrcSpan -> SrcSpan
adjustSpan removed x@(SrcSpan f sl sc el ec) =
    let (sl', sc') = adjustPos removed (sl, sc)
        (el', ec') = adjustPos removed (el, ec) in
    SrcSpan f sl' sc' el' ec'

removeComment :: Comment -> Module SrcSpanInfo -> Module SrcSpanInfo
removeComment (Comment b l s) m@(Module l' h ps is ds) =
  everywhere (mkT (adjustSpan l)) m

-- | Partition a module's declarations according to the graph of connected components
-- in the "declares - uses" graph.
partitionDecls ::
    forall l. (Data l, Eq l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> [Set (Decl (Scoped l))]
partitionDecls i = do
    (fst . runGraph) $ do
      g <- makeUsesGraph i
      (tmp :: [[Decs (Scoped l)]]) <- sequence (fmap (mapM (labNode g)) (components g))
      return $ fmap (Set.fromList . concat . fmap unDecs) tmp

-- | Build a graph whose nodes are declaration groups and whose edges
-- are the "declares, uses" relation.  Each edge is labeled with a set
-- of symbols.
makeUsesGraph ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> State (NodeMap (Decs (Scoped l))) (Gr (Decs (Scoped l)) (Set Symbol))
makeUsesGraph i =
    mkGraphM declGroups (concatMap (\a -> mapMaybe (edge a) declGroups) declGroups)
    where
      declGroups :: [Decs (Scoped l)]
      declGroups = groupDecs i (getModuleDecls (_module i))
      -- Create edges from any declaration A to any other declaration
      -- B such that A declares a symbol that B uses.
      edge :: Decs (Scoped l) -> Decs (Scoped l) -> Maybe (Decs (Scoped l), Decs (Scoped l), Set Symbol)
      edge a b = if Set.null common then Nothing else Just (a, b, common)
          where common = Set.intersection (declares i (unDecs' a)) (uses i b)
      unDecs' :: Decs (Scoped l) -> Set (Decl (Scoped l))
      unDecs' = Set.fromList . unDecs

-- | Declarations come in sets - a signature, followed by one or more
-- Decls.
groupDecs :: forall l. (Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> [Decl (Scoped l)] -> [Decs (Scoped l)]
groupDecs _ [] = error "makeDecs - invalid argument"
groupDecs i (d1 : ds1) =
    -- With foldr we encounter the declarations in reverse, so the
    -- signature ends up with the previous declaration.
    snd $ foldl go (getTopDeclSymbols' i d1, [Decs [d1]]) ds1
    where
      go :: (Set Symbol, [Decs (Scoped l)]) -> Decl (Scoped l) -> (Set Symbol, [Decs (Scoped l)])
      go (_, []) _ = (Set.empty, [])
      go (_ss, Decs ds : more) d@(TypeSig {}) =
        -- Assuming signature comes first - is this a bad assumption?
          (trace "sig" (mempty, Decs [d] : Decs ds : more))
      -- If the symbol set is empty we have seen a signature, this must(?)
      -- be the corresponding declaration.
      go (ss, Decs ds : more) d | Set.null ss =
          let ss' = getTopDeclSymbols' i d in
          (trace "dec after sig" (Set.union ss ss', Decs (ds ++ [d]) : more))
      go (ss, Decs ds : more) d =
          let ss' = getTopDeclSymbols' i d in
          if Set.null (Set.intersection ss ss')
          then (trace "new dec or sig" (ss', Decs [d] : Decs ds : more))
          else (trace "additional dec" (Set.union ss ss', Decs (ds ++ [d]) : more))

-- | Given a module, generate a new module which only has the given
-- declarations, assumed to be a subset of the declarations from the
-- original.  Besides updating the declaration list, this must filter
-- the exports and strip out comments associated with deleted decls or
-- exports.
putModuleDecls ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> (Decl (Scoped l) -> Bool)
    -> (ExportSpec (Scoped l) -> Bool)
    -> (Comment -> Bool)
    -> ModuleInfo (Scoped l)
putModuleDecls i@(ModuleInfo {_module = Module l h ps is ds, _moduleComments = cs})
               selectedDecls selectedExports selectedComments =
    i { _module = Module l (fmap doHead h) ps is (filter selectedDecls ds),
        _moduleComments = filter (selectedComments) cs}
    where doHead (ModuleHead l n w me) = ModuleHead l n w (fmap doSpecs me)
          doSpecs (ExportSpecList l' es) = ExportSpecList l' (filter selectedExports es)
putModuleDecls i@(ModuleInfo {_module = m}) _ _ _ = i

filterExports ::
    forall l. (Data l, Ord l, Show l)
    => Set (ExportSpec (Scoped l))
    -> Maybe (ModuleHead (Scoped l))
    -> Maybe (ModuleHead (Scoped l))
filterExports _ Nothing = Nothing
filterExports _ (Just (ModuleHead l n w Nothing)) = Just (ModuleHead l n w Nothing)
filterExports selectedExports (Just (ModuleHead l n w (Just (ExportSpecList l' es)))) =
    Just (ModuleHead l n w (Just (ExportSpecList l' (filter (`Set.member` selectedExports) es))))

-- | Return the set of ExportSpec that are not supplied by the set of
-- declarations.
exportsToKeep ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
exportsToKeep i@(ModuleInfo {_module = Module _ (Just (ModuleHead _ _ _(Just (ExportSpecList _ es)))) _ _ _}) ds  =
  foldl go Set.empty es
  where
    syms = declares i ds
    go r e@(EVar _l qname) =
        if any (`member` syms) (lookupName qname (_moduleGlobals i)) then Set.insert e r else r
    go r e@(EThingWith _l _w qname _cname) =
       if any (`member` syms) (lookupName qname (_moduleGlobals i)) then Set.insert e r else r
    go r e@(EAbs _l _ns _name) = Set.insert e r
    go r e@(EModuleContents _ _) = Set.insert e r
exportsToKeep _ _ = Set.empty

-- | Symbols declared by a declaration.
declares :: forall l. (Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> Set (Decl (Scoped l)) -> Set Symbol
declares i ds = flatten (Set.map (getTopDeclSymbols' i) ds)

flatten :: Ord a => Set (Set a) -> Set a
flatten = foldl Set.union mempty

-- | Symbols used in a declaration - a superset of declares.
uses :: forall l d. (Data d, Data l, Ord l, Show l) => ModuleInfo (Scoped l) -> d -> Set Symbol
uses i b = Set.fromList (concatMap (`lookupName` (_moduleGlobals i)) names)
    where
      names :: [QName (Scoped l)]
      names = fmap nameToQName (gFind b :: [Name (Scoped l)]) ++ gFind b :: [QName (Scoped l)]
