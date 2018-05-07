{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
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
    , decomposeModule
    ) where

import Control.Monad.State (State)
import Data.Generics (Data, Typeable)
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
import Refactor.Utils (con, EZPrint(ezPrint), gFind)

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

putNewModules :: ModuleInfo SrcSpanInfo -> [Module SrcSpanInfo] -> IO ()
putNewModules i ms =
    mapM_ (\(m, n) -> writeFile ("Tmp" ++ show n ++ ".hs") (exactPrint m (_moduleComments i))) (zip ms [1..])

-- Filter some declarations out of a module.  To do this we build a
-- graph of the "uses" relation on the module's declaration.  The
-- result is the declarations reachable in the graph and the graph's
-- complement.
--
--     λ> [i] <- loadModules def [ModuleFilePath (Just "src") "Refactor/Graph.hs"]
--     λ> let s = Value {symbolModule = ModuleName () "Refactor.Graph", symbolName = Language.Haskell.Exts.Ident () "findModuleByKeyUnsafe"}
--     λ> putNewModules (fmap unScope i) (fmap (fmap unScope) (decomposeModule (\d -> member s (getTopDeclSymbols' i d)) i))
decomposeModule ::
    forall l. (Data l, Eq l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> [Module (Scoped l)]
decomposeModule i@(ModuleInfo {_module = Module _l _h _ps _is _ds}) =
    fmap (uncurry (putModuleDecls i)) (zip partitionedDecls deletedExports)
    where
      deletedExports = fmap (exportsToRemove i) partitionedDecls
      partitionedDecls = partitionDecls i
decomposeModule (ModuleInfo {_module = m}) = [m]

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
    -- -> Module (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
    -> Module (Scoped l)
putModuleDecls (ModuleInfo {_module = Module l h ps is ds}) filteredDecls removedExports =
    Module l (filterExports removedExports h) ps is (filter (`Set.member` filteredDecls) ds)
putModuleDecls (ModuleInfo {_module = m}) _ _ = m

filterExports ::
    forall l. (Data l, Ord l, Show l)
    => Set (ExportSpec (Scoped l))
    -> Maybe (ModuleHead (Scoped l))
    -> Maybe (ModuleHead (Scoped l))
filterExports _ Nothing = Nothing
filterExports _ (Just (ModuleHead l n w Nothing)) = Just (ModuleHead l n w Nothing)
filterExports removedExports (Just (ModuleHead l n w (Just (ExportSpecList l' es)))) =
    Just (ModuleHead l n w (Just (ExportSpecList l' (filter (`Set.notMember` removedExports) es))))

-- | Return the set of ExportSpec that are not supplied by the set of
-- declarations.
exportsToRemove ::
    forall l. (Data l, Ord l, Show l)
    => ModuleInfo (Scoped l)
    -> Set (Decl (Scoped l))
    -> Set (ExportSpec (Scoped l))
exportsToRemove i@(ModuleInfo {_module = Module _ (Just (ModuleHead _ _ _(Just (ExportSpecList _ es)))) _ _ _}) ds  =
  foldl go Set.empty es
  where
    syms = declares i ds
    go r e@(EVar _l qname) =
        if any (`member` syms) (lookupName qname (_moduleGlobals i)) then r else Set.insert e r
    go r e@(EThingWith _l _w qname _cname) =
       if any (`member` syms) (lookupName qname (_moduleGlobals i)) then r else Set.insert e r
    go r (EAbs _l _ns _name) = r
    go r (EModuleContents _ _) = r
exportsToRemove _ _ = Set.empty

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
