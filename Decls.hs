{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
module Decls (moveDeclsByName, moveInstDecls, moveSpliceDecls, instClassPred,
              runSimpleMove, runSimpleMoveUnsafe, runMoveUnsafe, moveDeclsAndClean, moveDecls,
              MoveSpec(MoveSpec), applyMoveSpec, traceMoveSpec) where

import Control.Exception (SomeException)
import Control.Lens (makeLenses, view)
import Control.Monad (foldM, void, when)
import Control.Monad.RWS (modify, MonadWriter(tell))
import Control.Monad.State (execState, MonadState)
import Data.Default (def)
import Data.Foldable as Foldable (find)
import Data.List ((\\), foldl', intercalate, nub, stripPrefix)
import Data.Map as Map (insertWith, Map, mapWithKey, singleton, toList)
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set, toList)
import Data.Tuple (swap)
import Debug.Trace (trace)
import GHC (GHCOpts(hsSourceDirs))
import Imports (cleanImports)
import qualified Language.Haskell.Exts.Annotated as A (Annotated(ann), Decl(InstDecl, TypeSig), ExportSpec, ExportSpecList(ExportSpecList), ImportDecl(importModule, importSpecs), ImportSpec, ImportSpecList(ImportSpecList), InstHead(..), InstRule(IParen, IRule), Module(Module), ModuleHead(ModuleHead), ModulePragma(..), Pretty, QName, Type)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sModuleName, sModulePragma, sQName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintStyleMode)
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(..), ImportDecl(..), ImportSpec(IThingAll, IThingWith, IVar), ModuleName(..), ModulePragma(..), Name(..), QName(Qual, Special, UnQual), Exp, Decl(SpliceDecl), Exp(SpliceExp), Splice(IdSplice, ParenSplice))
import Language.Haskell.Names (Environment, resolve, Symbol(..))
import LoadModule (loadModule, loadModules)
import ModuleInfo (ModuleInfo(..))
import ModuleKey (moduleFullPath, ModuleKey(..), moduleName)
import SrcLoc (endLoc, endOfHeader, endOfImports, keep, keepAll, scanModule, skip, textOfSpan, srcLoc, ScanM, withTrailingWhitespace)
import Symbols (FoldDeclared(foldDeclared), toExportSpecs)
import System.FilePath.Find as FilePath ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import Text.PrettyPrint (mode, Mode(OneLineMode), style)
import Utils (dropWhile2, EZPrint(ezPrint), gFind, listPairs, replaceFile, withCleanRepo, withCurrentDirectory, withTempDirectory)

-- | Specifies where to move each declaration of each module.  Given a
-- departure module key and a declaration, return an arrival module key.
newtype MoveSpec = MoveSpec (ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey)

instance Monoid MoveSpec where
    mempty = MoveSpec $ \k _ -> k
    mappend (MoveSpec f) (MoveSpec g) = MoveSpec $
      \k0 d ->
        let k1 = f k0 d
            k2 = g k0 d in
        if k1 == k0
        then k2
        else if k2 == k0
             then k1
             else if k1 == k2
                  then k1
                  else error "Conflicting move specs"

data Rd
    = Rd { _moveSpec :: MoveSpec
         , _modules :: [ModuleInfo]
         , _environment :: Environment }

$(makeLenses ''Rd)

applyMoveSpec :: MoveSpec -> ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey
applyMoveSpec (MoveSpec f) k d = f k d

traceMoveSpec :: MoveSpec -> MoveSpec
traceMoveSpec (MoveSpec f) = MoveSpec $ \k d ->
  let k' = f k d in
  if k /= k'
  then (trace ("moveSpec " ++ show k ++ " d -> " ++ show k') k')
  else k'

-- | Declaration moves can be characterized as one of two types, Down
-- or Up.  This must be computed by scanning the parsed code of the
-- departure module (the module where the declaration is when we
-- begin) for any remaining uses of the declaration's symbols.  Note
-- that it is possible to specify a move that results in a legitimate
-- import loop.  The only solution to this is to bring more
-- declarations over, or some manual intervention.
data MoveType
    = Down
    -- ^ A Down move moves a declaration away from where it is used,
    -- which means we probably need to add imports of the symbols of
    -- the declaration to the departure module.
    | Up
    -- ^ An Up move moves a declaration towards where it is used.  In
    -- this case leaving behind an import will probably create an
    -- import cycle.  Therefore we need to convert the (remaining)
    -- exports of the departure module into imports and add them to
    -- the arrival module.
    deriving Show

-- A simple MoveSpec builder.
moveDeclsByName :: String -> String -> String -> MoveSpec
moveDeclsByName symname modname modname' = MoveSpec $
    \mkey decl ->
        let syms = foldDeclared Set.insert mempty decl in
        case mkey of
          ModuleKey {_moduleName = S.ModuleName name}
              | name == modname && (Set.member (S.Ident symname) syms || Set.member (S.Symbol symname) syms) ->
                  mkey {_moduleName = S.ModuleName modname'}
          _ -> mkey

moveInstDecls :: (ModuleKey -> A.QName SrcSpanInfo -> [A.Type SrcSpanInfo] -> ModuleKey) -> MoveSpec
moveInstDecls instpred =
    MoveSpec f
    where
      f mkey (A.InstDecl _ _ irule _) = g mkey irule
      f mkey _ = mkey
      g mkey (A.IParen _ irule) = g mkey irule
      g mkey (A.IRule _ _ _ ihead) = uncurry (instpred mkey) (h [] ihead)
      h :: [A.Type SrcSpanInfo] -> A.InstHead SrcSpanInfo -> (A.QName SrcSpanInfo, [A.Type SrcSpanInfo])
      h types (A.IHParen _ ihead) = h types ihead
      h types (A.IHApp _ ihead typ) = h (typ : types) ihead
      h types (A.IHCon _ name) = (name, types)
      h types (A.IHInfix _ typ name) = (name, typ : types)
{-
        let syms = foldDeclared Set.insert mempty decl in
        case mkey of
          ModuleKey {_moduleName = S.ModuleName aname}
              | aname == mname && (Set.member (S.Ident fname) syms || Set.member (S.Symbol fname) syms) ->
                  mkey {_moduleName = S.ModuleName mname'}
          _ -> mkey
-}

moveSpliceDecls :: (ModuleKey -> S.Exp -> ModuleKey) -> MoveSpec
moveSpliceDecls exppred =
    MoveSpec (\k d -> f k (sDecl d))
    where
      f :: ModuleKey -> S.Decl -> ModuleKey
      f mkey (S.SpliceDecl _ exp') = g mkey exp'
      f mkey _ = mkey
      g mkey (S.SpliceExp splice) = h mkey splice
      g mkey _ = mkey
      h mkey (S.IdSplice _s) = mkey
      h mkey (S.ParenSplice exp') = exppred mkey exp'

-- | Build the argument to moveInstDecls
instClassPred :: String -> String -> String ->
                 ModuleKey -> A.QName SrcSpanInfo -> [A.Type SrcSpanInfo] -> ModuleKey
instClassPred classname depart arrive key@(ModuleKey {_moduleName = mname}) qname _ts
    | mname == S.ModuleName depart &&
      (gFind (sQName qname) :: [S.Name]) == [S.Ident classname] =
        key {_moduleName = S.ModuleName arrive}
instClassPred _ _ _ key _ _ = key

prettyPrint' :: A.Pretty a => a -> String
prettyPrint' = prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode

-- | Run moveDeclsAndClean on all the .hs files in the given
-- directory.
runSimpleMove :: FilePath -> MoveSpec -> IO ()
runSimpleMove top spec = withCleanRepo $ runSimpleMoveUnsafe top spec

runSimpleMoveUnsafe :: FilePath -> MoveSpec -> IO ()
runSimpleMoveUnsafe top mv =
    withCurrentDirectory top $
    withTempDirectory True "." "scratch" $ \scratch -> do
      paths <- (catMaybes . map (stripPrefix "./"))
               <$> (FilePath.find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
      loadModules def paths >>= moveDeclsAndClean mv scratch ["."]

runMoveUnsafe :: FilePath -> [FilePath] -> MoveSpec -> IO ()
runMoveUnsafe top hsSourceDirs mv =
    withCurrentDirectory top $
    withTempDirectory True "." "scratch" $ \scratch -> do
      paths <- (catMaybes . map (stripPrefix "./"))
               <$> (FilePath.find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
      loadModules def paths >>= moveDeclsAndClean mv scratch hsSourceDirs

moveDeclsAndClean :: MoveSpec -> FilePath -> [FilePath] -> [ModuleInfo] -> IO ()
moveDeclsAndClean mv scratch hsSourceDirs mods = do
  -- Move the declarations and rewrite the updated modules
  let env = resolve (map _module mods) mempty
      rd = t1 (Rd mv mods env)
  oldPaths <-
      mapM (\(m, s) -> do
              let p = _modulePath m
              case _moduleText m == s of
                True -> return Nothing
                False -> replaceFile p s >> return (Just p))
           (zip mods (moveDecls rd))
  newPaths <- mapM (\(k, s) -> do
                      let path = moduleFullPath k
                      void $ replaceFile (trace ("New file: " ++ show path) path) s
                      pure path)
                   (Map.toList (newModuleMap rd))
  -- Re-read the updated modules and clean their imports
  -- (Later we will need to find the newly created modules here)
  modules' <- mapM (\p -> either (loadError p) id <$> loadModule def p) (catMaybes oldPaths ++ newPaths) :: IO [ModuleInfo]
  cleanImports scratch (def {hsSourceDirs = hsSourceDirs}) modules'
    where
      loadError :: FilePath -> SomeException -> ModuleInfo
      loadError p e = error ("Unable to load updated module " ++ show p ++ ": " ++ show e)
      t1 rd@(Rd _ _ env) = trace ("environment: " ++ show env) rd

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: Rd -> [String]
moveDecls rd =
    map (\info -> moveDeclsOfModule rd info) (view modules rd)

-- | Update one module and return its text
moveDeclsOfModule :: Rd -> ModuleInfo -> String
moveDeclsOfModule rd info@(ModuleInfo {_module = A.Module l _ ps _ _}) =
    scanModule (do keep (srcLoc l)
                   tell (newPragmas rd (_moduleKey info) ps)
                   updateHeader rd info
                   updateImports rd info
                   updateDecls rd info
                   keepAll)
               info
moveDeclsOfModule _ x = error $ "moveDeclsOfModule - unexpected module: " ++ show (_module x)

-- | Unsafe ModuleInfo lookup
findModuleByKey :: [ModuleInfo] -> ModuleKey -> Maybe ModuleInfo
findModuleByKey mods thisKey = Foldable.find (\m -> _moduleKey m == thisKey) mods

findModuleByKeyUnsafe :: [ModuleInfo] -> ModuleKey -> ModuleInfo
findModuleByKeyUnsafe mods thisKey = maybe (error $ "Module not found: " ++ show thisKey) id $ findModuleByKey mods thisKey

exportSep :: String
exportSep = "\n    , "

-- | Build the new modules
newModuleMap :: Rd -> Map ModuleKey String
newModuleMap rd@(Rd mv mods _env) =
    textMap
    where
      textMap :: Map ModuleKey String
      textMap = Map.mapWithKey (\k ds -> doModule k ds) declMap
          where
            doModule :: ModuleKey -> [(ModuleKey, A.Decl SrcSpanInfo)] -> String
            -- Build module thiskey from the list of (fromkey, decl) pairs
            doModule thisKey pairs =
                newPragmas rd thisKey [] ++
                "module " ++ maybe "Main" prettyPrint (moduleName thisKey) ++ "(" ++
                intercalate exportSep (newExports mv mods thisKey) ++ "\n    ) where\n\n" ++
                concatMap (\(someKey, _ds) -> importsForArrivingDecls rd thisKey [] (findModuleByKeyUnsafe mods someKey)) pairs ++
                -- importsForDepartingDecls mv mods thisKey ++  new module, no decls can depart
                newDecls mv mods thisKey
      declMap :: Map ModuleKey [(ModuleKey, A.Decl SrcSpanInfo)]
      declMap = foldl' doModule mempty mods
          where
            doModule :: Map ModuleKey [(ModuleKey, A.Decl SrcSpanInfo)] -> ModuleInfo -> Map ModuleKey [(ModuleKey, A.Decl SrcSpanInfo)]
            doModule mp (ModuleInfo {_module = A.Module _ _ _ _ ds, _moduleKey = k}) = foldl' (doDecl k) mp ds
            doModule mp _ = mp
            doDecl k mp d = let k' = applyMoveSpec mv k d in
                            if Set.member k' oldKeys then mp else Map.insertWith (++) k' [(k, d)] mp
      oldKeys :: Set ModuleKey
      oldKeys = Set.fromList (map _moduleKey mods)

-- | What is the best place to put a newly created module?
defaultHsSourceDir :: [ModuleInfo] -> FilePath
defaultHsSourceDir mods =
    let countMap = foldl' (\mp path -> Map.insertWith (+) path (1 :: Int) mp)
                          (Map.singleton "." 0)
                          (map _modulePath mods) in
    snd (maximum (map swap (Map.toList countMap)))

-- If a declaration is arriving in this module we need to add all the LANGUAGE
-- pragmas from that module to this one.
newPragmas :: Rd -> ModuleKey -> [A.ModulePragma SrcSpanInfo] -> String
newPragmas (Rd mv mods _env) thisKey thesePragmas =
  let (arriving :: Set S.ModulePragma) =
          execState (mapM_ (\(ModuleInfo {_moduleKey = someKey,_module = (A.Module _ _mh somePragmas _is someDecls)}) ->
                                when
                                  (someKey /= thisKey)
                                  (mapM_ (\d -> when
                                                  (applyMoveSpec mv someKey d == thisKey)
                                                  (mapM_ addPragma (pragmaDiff somePragmas thesePragmas))) someDecls)) mods) mempty in
  unlines (map prettyPrint' (Set.toList arriving))
    where
      addPragma :: MonadState (Set S.ModulePragma) m => S.Name -> m ()
      addPragma name = modify (Set.insert (S.LanguagePragma (SrcLoc "" 1 1) [name]))
      pragmaDiff :: [A.ModulePragma SrcSpanInfo] -> [A.ModulePragma SrcSpanInfo] -> [S.Name]
      pragmaDiff ps qs = t1 (concatMap pragmaLanguageNames (map sModulePragma ps) \\ concatMap pragmaLanguageNames (map sModulePragma qs))
          where t1 x = trace ("pragmaDiff " ++ show (map sModulePragma ps) ++ " " ++ show (map sModulePragma qs) ++ " -> " ++ show x) x
      pragmaLanguageNames :: S.ModulePragma -> [S.Name]
      pragmaLanguageNames (S.LanguagePragma _l names) = names
      pragmaLanguageNames _ = []

-- | Write the new export list.  Exports of symbols that have moved
-- out are removed.  Exports of symbols that have moved in are added
-- *if* the symbol is imported anywhere else.
updateHeader :: Rd -> ModuleInfo -> ScanM ()
updateHeader (Rd mv _mods _env)
             i@(ModuleInfo {_moduleKey = k,
                            _module = m@(A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ specs)))) _ _ _)}) = do
  maybe (pure ()) (keep . srcLoc . A.ann) (listToMaybe specs)
  foldM doExport False specs
  keep (endOfHeader m)
    where
      doExport :: Bool -> A.ExportSpec SrcSpanInfo -> ScanM Bool
      doExport needSep spec =
          case findNewKeyOfExportSpec mv i spec of
            Just k' | k' /= k -> skip (endLoc (A.ann spec)) >> pure needSep
            _ | needSep -> keep (endLoc (A.ann spec)) >> pure True
            _ -> skip (srcLoc (A.ann spec)) >> keep (endLoc (A.ann spec)) >> pure True

updateHeader _ _ = pure ()

-- | Text of exports added due to arriving declarations
newExports :: MoveSpec -> [ModuleInfo] -> ModuleKey -> [String]
newExports mv mods thisKey =
    map prettyPrint names
    where
      -- (This should be inferred from the text of thisKey.)
      names = nub $ concatMap newExportsFromModule (filter (\x -> _moduleKey x /= thisKey) mods)
      -- Scan a module other than thisKey for declarations moving to thisKey.  If
      -- found, transfer the export from there to here.
      newExportsFromModule :: ModuleInfo -> [S.ExportSpec]
      newExportsFromModule (ModuleInfo {_moduleKey = k', _module = A.Module _ _ _ _ ds}) =
          concatMap (\d -> if (applyMoveSpec mv k' d == thisKey) then toExportSpecs d else []) ds
      newExportsFromModule x = error $ "newExports - unexpected module: " ++ show (_module x)

findNewKeyOfExportSpec :: MoveSpec -> ModuleInfo -> A.ExportSpec SrcSpanInfo -> Maybe ModuleKey
findNewKeyOfExportSpec mv info@(ModuleInfo {_moduleKey = k}) spec =
    fmap (applyMoveSpec mv k) (findDeclOfExportSpec info spec)

-- | Find the declaration that causes all the symbols in the
-- ImportSpec to come into existance.
findDeclOfExportSpec :: ModuleInfo -> A.ExportSpec SrcSpanInfo -> Maybe (A.Decl SrcSpanInfo)
findDeclOfExportSpec info spec =
    findDeclOfSymbols info (foldDeclared Set.insert mempty spec)
    where
      findDeclOfSymbols :: ModuleInfo -> Set S.Name -> Maybe (A.Decl SrcSpanInfo)
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ _}) syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ decls}) syms =
          case filter (isSubsetOf syms . foldDeclared Set.insert mempty) (filter notSig decls) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map srcLoc ds)
      findDeclOfSymbols x _ = error $ "findDeclOfExportSpec - unexpected module: " ++ show (_module x)

-- Find the declaration of the symbols of an import spec.  If that
-- declaration moved, update the module name.
updateImports :: Rd -> ModuleInfo -> ScanM ()
updateImports rd@(Rd _mv mods _env) (ModuleInfo {_moduleKey = thisKey, _module = (A.Module _ _ _ thisModuleImports _)}) = do
  -- Update the existing imports
  mapM_ (uncurry doImportDecl) (listPairs thisModuleImports)
  mapM_ doNewImports (filter (\m' -> _moduleKey m' /= thisKey) mods)
  tell $ importsForDepartingDecls rd (findModuleByKey mods thisKey)
    where
      -- Process one import declaration.  Each of its specs will
      -- either be kept, discarded, or moved to a new import with a
      -- new module name.
      doImportDecl :: Maybe (A.ImportDecl SrcSpanInfo) -> Maybe (A.ImportDecl SrcSpanInfo) -> ScanM ()
      doImportDecl Nothing Nothing = pure ()
      doImportDecl Nothing (Just first) = keep (srcLoc first)
      doImportDecl (Just x) next = do
        case A.importSpecs x of
          Nothing -> do
            keep (endLoc (A.ann x))
            withTrailingWhitespace keep (fmap srcLoc next)
          Just (A.ImportSpecList l hiding specs) -> do
            keep $ case specs of
                     (spec : _) -> srcLoc (A.ann spec)
                     [] -> srcLoc l
            mapM_ (uncurry (doImportSpec (sModuleName (A.importModule x)) hiding)) (listPairs specs)
            keep (endLoc l)
            withTrailingWhitespace keep (fmap srcLoc next)
            -- Some of the specs may be of declarations that moved out of thisKey,
            -- output new imports for these.
            mapM_ (importsForMovingDecls (sModuleName (A.importModule x)) hiding) specs

      doImportSpec :: S.ModuleName -> Bool -> Maybe (A.ImportSpec SrcSpanInfo) -> Maybe (A.ImportSpec SrcSpanInfo) -> ScanM ()
      doImportSpec _ _ Nothing _ = pure ()
      doImportSpec name hiding (Just spec) next =
          case newModuleOfImportSpec rd name spec of
            Just name'
                | -- Retain import if module name is unchanged
                  name' == name -> keep nextLoc
            _ -> skip nextLoc
          where
            nextLoc = maybe (endLoc (A.ann spec)) (srcLoc . A.ann) next

      importsForMovingDecls :: S.ModuleName -> Bool -> A.ImportSpec SrcSpanInfo -> ScanM ()
      importsForMovingDecls name hiding spec =
          case newModuleOfImportSpec rd name spec of
            Just name'
                | -- has import module name changed?
                  name' /= name &&
                  -- don't generate self imports
                  name' /= thisModuleName ->
                    tell ("\nimport " ++ prettyPrint' name' ++ " (" ++ prettyPrint' spec ++ ")" ++ "\n")
            _ -> pure ()

      thisModuleName = case thisKey of
                         ModuleKey {_moduleName = x} -> x
                         _ -> S.ModuleName "Main"
      -- Add new imports due to declarations moving from someKey to
      -- thisKey.  All of the imports in someKey must be duplicated in
      -- thisKey (except for imports of thisKey).  Also, all of the
      -- exports in someKey must be turned into imports to thisKey
      -- (with updated module names.)  Finally, if a declaration
      -- moves from thisKey to someKey, we need to add an import of it
      -- here (as long as someKey doesn't import thisKey)
      doNewImports :: ModuleInfo -> ScanM ()
      doNewImports someModule = do
        tell $ importsForArrivingDecls rd thisKey thisModuleImports someModule
updateImports _ x = error $ "updateImports - unexpected module: " ++ show (_module x)

-- | If a decl is move from thisModule to someModule we may need to
-- import its symbols to satisfy remaining declarations that use it.
-- However, if someModule already imports thisModule that will cause
-- an import cycle.  Unfortunately, even if someModule *does not* import
-- thisModule a cycle can appear, because of the code that converts the
-- exports of someModule into imports in thisModule.
importsForDepartingDecls :: Rd -> Maybe ModuleInfo -> String
importsForDepartingDecls rd@(Rd mv mods _env) (Just (ModuleInfo {_moduleKey = thisKey@(ModuleKey {_moduleName = thisModuleName}), _module = A.Module _ _ _ _ ds})) =
    concatMap (\d -> case applyMoveSpec mv thisKey d of
                       someKey@(ModuleKey {_moduleName = someModuleName})
                           | someKey /= thisKey ->
                               case t2 d thisKey someKey (findModuleByKey mods someKey) of
                                 Just (ModuleInfo {_module = A.Module _ _ _ someModuleImports _}) ->
                                     case moveType rd someModuleImports thisModuleName of
                                       Down -> "\n" ++ prettyPrint' (importSpecFromDecl someModuleName d) ++ "\n"
                                       Up -> ""
                                 -- Moving a declaration from thisKey to someKey is a "Down"
                                 -- move if there are any remaining uses of those symbols in
                                 -- thisKey.  It is an "Up" move if the declarations still
                                 -- depend on exports of thisKey.  FIXME: I haven't
                                 -- implemented theses tests yet, so assume Down for now.
                                 Just _ -> error "Unexpected module"
                                 Nothing -> "\n" ++ prettyPrint' (importSpecFromDecl someModuleName d) ++ "\n"
                       _ -> "") ds
    where
      t2 d k k' x = trace ("departing: " ++ ezPrint d ++ " from " ++ ezPrint k ++ " -> " ++ ezPrint k' ++ ", " ++ moveType' rd k k') x

importsForDepartingDecls _ _ = ""

moveType' :: Rd -> ModuleKey -> ModuleKey -> String
moveType' rd@(Rd _mv mods _env) thisKey' someKey' =
    case (findModuleByKey mods someKey', moduleName thisKey') of
      (Just (ModuleInfo {_module = A.Module _ _ _ someModuleImports _}), Just thisModuleName') ->
          show (moveType rd someModuleImports thisModuleName')
      _ -> "Unknown"

-- | If a declaration move requires us to import the departure module
-- into the arrival module it is called an "Up" move (towards where it
-- is used, away from the declarations it uses.)  When a declaration
-- move requires us to add an import of the declaration's symbols in
-- the departure module it is a "Down" move, away from where it is
-- used and towards the declarations that it uses.  Making both these
-- additions would create an import cycle, so we need to figure out
-- which it is:
--
--   (1) If the arrival module already imports the departure module it
--       must be an Up move.
--   (2) If the symbol is still used in the departure module by some
--       declaration that is not moving out, or by some declaration
--       that is moving in, it is a Down move.  This is a difficult
--       computation, so for now we assume it is true unless explicitly
--       marked "Up".

moveType :: Rd -> [A.ImportDecl SrcSpanInfo] -> S.ModuleName -> MoveType
moveType rd@(Rd _ _ env) arrivalModuleImports departureModuleName =
    case arrivalModuleImports `importsSymbolsFrom` departureModuleName of
      True -> Up
      False -> Down

-- | Copy the entire import list of the departure module - it will be
-- cleaned up later.  Also, if this is an Up move we can import the
-- departure module itself.  If it is a down move the departure module
-- will be importing the arriving declaration.
importsForArrivingDecls :: Rd -> ModuleKey -> [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> String
importsForArrivingDecls rd@(Rd mv _ _) thisKey thisModuleImports someModule@(ModuleInfo {_moduleKey = someKey, _module = A.Module _ _ _ someModuleImports ds}) =
    if any (\d -> applyMoveSpec mv someKey d == thisKey) ds
    then concatMap
             (\i -> prettyPrint' i ++ "\n")
             (filter (\i -> moduleName thisKey /= Just (sModuleName (A.importModule i))) someModuleImports) ++
         concatMap
             (\i -> prettyPrint' i ++ "\n")
             (maybeToList (importDeclFromExportSpecs rd thisModuleImports someModule)) ++
         case (fmap (moveType rd someModuleImports) (moduleName thisKey), moduleName someKey) of
           (Just Up, Just someName) -> "import " ++ prettyPrint someName ++ "\n"
           _ -> ""
    else ""
importsForArrivingDecls _ _ _ _ = ""

-- | If a declaration moves from someModule to thisModule, and nothing
-- in thisModule is imported by someModule, add imports to thisModule
-- of all the symbols exported by someModule.
importDeclFromExportSpecs :: Rd -> [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> Maybe S.ImportDecl
importDeclFromExportSpecs rd
                          thisModuleImports
                          someInfo@(ModuleInfo
                                    {_moduleKey = someKey@(ModuleKey {_moduleName = someModuleName}),
                                     _module = someModule@(A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ especs@(_ : _))))) _ _ _)}) =
    -- Do we need to import the remaining exports from the departure
    -- module into the arrival module?  Only if we are moving the
    -- declaration 'Up', which implies that it may use symbols from
    -- the departure module.
    case moveType rd thisModuleImports someModuleName of
      Up -> Just (S.ImportDecl { S.importLoc = srcLoc someModule
                               , S.importModule = maybe (S.ModuleName "Main") id (moduleName someKey)
                               , S.importQualified = False
                               , S.importSrc = False
                               , S.importSafe = False
                               , S.importPkg = Nothing
                               , S.importAs = Nothing
                               , S.importSpecs = Just (False, importSpecsFromExportSpecs) })
      Down -> Nothing
    where
      -- Build ImportSpecs of m corresponding to some export specs.
      importSpecsFromExportSpecs :: [S.ImportSpec]
      importSpecsFromExportSpecs =
          map exportToImport' (filter (\e -> findNewKeyOfExportSpec (view moveSpec rd) someInfo e == Just (_moduleKey someInfo)) especs)
          where
importDeclFromExportSpecs _ _ _ = Nothing

-- | These cases probably need work.
exportToImport' :: A.ExportSpec SrcSpanInfo -> S.ImportSpec
exportToImport' = exportToImport . sExportSpec

exportToImport :: S.ExportSpec -> S.ImportSpec
exportToImport (S.EVar (S.Qual _mname name)) = S.IVar name
exportToImport (S.EVar (S.UnQual name)) = S.IVar name
exportToImport x@(S.EVar (S.Special _)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(S.EAbs _space (S.UnQual _name)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(S.EAbs _space (S.Qual _mname _name)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(S.EAbs _space (S.Special _)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport (S.EThingAll (S.UnQual name)) = S.IThingAll name
exportToImport x@(S.EThingAll (S.Qual _mname _name)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(S.EThingAll (S.Special _)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport (S.EThingWith (S.UnQual name) cnames) = S.IThingWith name cnames
exportToImport x@(S.EThingWith (S.Qual _mname _name) _cnames) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(S.EThingWith (S.Special _) _cnames) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(S.EModuleContents _) = error $ "exportToImport: " ++ prettyPrint x
-- exportToImport x = error $ "exportToImport: " ++ prettyPrint x

-- | Does module m import from name?
importsSymbolsFrom :: [A.ImportDecl SrcSpanInfo] -> S.ModuleName -> Bool
importsSymbolsFrom imports importee = any (\i -> sModuleName (A.importModule i) == importee) imports

-- | Build an ImportDecl that imports the symbols of d from m.
importSpecFromDecl :: S.ModuleName -> A.Decl SrcSpanInfo -> S.ImportDecl
importSpecFromDecl m d =
    S.ImportDecl { S.importLoc = srcLoc d
                 , S.importModule = m
                 , S.importQualified = False
                 , S.importSrc = False
                 , S.importSafe = False
                 , S.importPkg = Nothing
                 , S.importAs = Nothing
                 , S.importSpecs = Just (False, map exportToImport (toExportSpecs d)) }

-- | Given in import spec and the name of the module it was imported
-- from, return the name of the new module where it will now be
-- imported from.
newModuleOfImportSpec :: Rd -> S.ModuleName -> A.ImportSpec SrcSpanInfo -> Maybe S.ModuleName
newModuleOfImportSpec (Rd mv mods _env) oldModname spec =
    case findModuleByName mods oldModname of
      Just info -> case findDeclOfImportSpec info spec of
                     Just d -> moduleName (applyMoveSpec mv (_moduleKey info) d)
                     -- Everything else we can leave alone - even if we can't
                     -- find a declaration, they might be re-exported.
                     Nothing {- | isReexport info spec -} -> Just oldModname
                     -- Nothing -> trace ("Unable to find decl of " ++ prettyPrint' spec ++ " in " ++ show oldModname) Nothing
      -- If we don't know about the module leave the import spec alone
      Nothing -> Just oldModname

findModuleByName :: [ModuleInfo] -> S.ModuleName -> Maybe ModuleInfo
findModuleByName mods oldModname =
    case filter (testModuleName oldModname) mods of
      [m] -> Just m
      [] -> Nothing
      _ms -> error $ "Multiple " ++ show oldModname
    where
      testModuleName :: S.ModuleName -> ModuleInfo -> Bool
      testModuleName modName (ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ name _ _)) _ _ _}) =
          sModuleName name == modName
      testModuleName modName (ModuleInfo {_module = A.Module _ Nothing _ _ _}) =
          modName == S.ModuleName "Main"
      testModuleName _ x = error $ "findModuleByName - unexpected module: " ++ show (_module x)

-- | Find the declaration in a module that causes all the symbols in
-- the ImportSpec to come into existance.
findDeclOfImportSpec :: ModuleInfo -> A.ImportSpec SrcSpanInfo -> Maybe (A.Decl SrcSpanInfo)
findDeclOfImportSpec info spec = findDeclOfSymbols info (foldDeclared Set.insert mempty spec)
    where
      findDeclOfSymbols :: ModuleInfo -> Set S.Name -> Maybe (A.Decl SrcSpanInfo)
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ _}) syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ decls}) syms =
          case filter (isSubsetOf syms . foldDeclared Set.insert mempty) (filter notSig decls) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map srcLoc ds)
      findDeclOfSymbols x _ = error $ "findDeclOfImportSpec - unexpected module: " ++ show (_module x)

notSig :: A.Decl t -> Bool
notSig (A.TypeSig {}) = False
notSig _ = True

#if 0
-- Are each of the symbols of this import spec re-exported by
-- some export spec info?
isReexport :: ModuleInfo -> A.ImportSpec SrcSpanInfo -> Bool
isReexport info@(ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ especs)))) _ _ _}) ispec =
    let syms = foldDeclared Set.insert mempty ispec in
    all (isReexported especs) syms
    -- not (null (filter (isReexported syms) especs))
    -- (not . null . filter (isReexport' syms . foldDeclared Set.insert mempty)) specs

-- Is symbol re-exported?
isReexported :: [A.ExportSpec SrcSpanInfo] -> S.Name -> Bool
isReexported specs sym = any (reexports sym) specs

-- Does this export spec re-export this symbol?  (FIXME: Need to
-- change sym type to handle EThingAll.)
reexports :: S.Name -> A.ExportSpec SrcSpanInfo -> Bool
reexports sym e@(A.EThingAll _ qname) = trace ("EThingAll") $ Set.member sym (foldDeclared Set.insert mempty e)
reexports sym (A.EModuleContents _ _mname) = False
reexports sym e = Set.member sym (foldDeclared Set.insert mempty e)
#endif

-- | Look through a module's imports, using findDeclOfImportSpec and
-- moveSpec to determine which refer to symbols that are moving from
-- one module to another.  There are three cases for an import that
-- moves.  It might move from another module to this module, in which
-- case it can be removed.  It might move between two modules other
-- than this one, in which case the a new import with the new module
-- name is added.  The final case is invalid - a module that imported
-- itself.
updateDecls :: Rd -> ModuleInfo -> ScanM ()
updateDecls (Rd mv mods _env) (ModuleInfo {_module = (A.Module _ _ _ _ decls), _moduleKey = thisKey}) = do
  -- keep (endOfImports m)
  -- Declarations that were already here and are to remain
  mapM_ doDecl (listPairs decls)
  keepAll
  tell $ newDecls mv mods thisKey
    where
      doDecl (Nothing, Nothing) = pure ()
      doDecl (Nothing, Just _first) = pure () -- keep (srcLoc first)
      doDecl (Just d, next) =
          case applyMoveSpec mv thisKey d of
            someKey | someKey /= thisKey -> do
              trace ("Moving " ++ ezPrint (foldDeclared (:) [] d) ++ " to " ++ ezPrint someKey ++ " from " ++ ezPrint thisKey) (pure ())
              skip (endLoc d)
              withTrailingWhitespace skip (fmap srcLoc next)
            _ -> do
              keep (endLoc d)
              withTrailingWhitespace keep (fmap srcLoc next)
updateDecls _ x = error $ "updateDecls - unexpected module: " ++ show (_module x)

-- | Declarations that are moving here from other modules.
newDecls :: MoveSpec -> [ModuleInfo] -> ModuleKey -> String
newDecls mv mods thisKey =
    concatMap doModule mods
    where
      -- Scan the declarations of all the modules except this one
      doModule info@(ModuleInfo {_module = (A.Module _l _mh _ps _is decls),
                                 _moduleKey = someKey})
          | someKey /= thisKey =
              scanModule (do skip (endOfImports (_module info))
                             mapM_ (uncurry (doDecl someKey)) (listPairs decls))
                         info
      doModule _ = ""

      -- If a declaration is to be moved to this module, extract its
      -- text and add it to the result.
      doDecl :: ModuleKey -> Maybe (A.Decl SrcSpanInfo) -> Maybe (A.Decl SrcSpanInfo) -> ScanM ()
      doDecl _ Nothing _ = pure ()
      doDecl someKey (Just d) next =
          case applyMoveSpec mv someKey d == thisKey of
            False -> do
              skip (endLoc d)
              withTrailingWhitespace skip (fmap srcLoc next)
            True -> do
              keep (endLoc d)
              withTrailingWhitespace keep (fmap srcLoc next)

-- | Get the text of a declaration including the preceding whitespace
declText :: ModuleInfo -> A.Decl SrcSpanInfo -> String
declText (ModuleInfo {_module = m@(A.Module _ mh ps is ds), _moduleText = mtext}) d =
    -- Find the end of the last object preceding d - could be the
    -- preceding declaration, the last import, the last pragma, or the
    -- module header.  If none of that exists use the module start
    -- location.
    let p = case ds of
              (d1 : _) | d == d1 -> endLoc (last (maybe [] (\x -> [A.ann x]) mh ++ map A.ann ps ++ map A.ann is))
              _ -> case dropWhile2 (\_  md2 -> Just d /= md2) ds of
                     (d1 : _) -> endLoc d1
                     [] -> srcLoc (A.ann m) in
    textOfSpan (mkSrcSpan p (endLoc d)) mtext
declText x _ = error $ "declText - unexpected module: " ++ show (_module x)

-- | Declaration predicates
testDeclaredNameString :: (String -> Bool) -> A.Decl SrcSpanInfo -> Bool
testDeclaredNameString p d = any p (gFind (toExportSpecs (sDecl d)) :: [String])
{-
testInstanceClass :: (String -> Bool) -> A.Decl SrcSpanInfo -> Bool
testInstanceClass p d@(A.InstDecl _mo _ir (Just idecls)) = testInstance

testInstance :: (A.InstDecl SrcSpanInfo -> Bool) -> A.Decl SrcSpanInfo -> Bool
testInstance p (A.InstDecl _mo _ir (Just idecls)) = any p idecls
testInstance _ _ _ = False
-}
