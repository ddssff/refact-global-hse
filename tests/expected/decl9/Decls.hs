{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}
module Decls (runSimpleMove, runSimpleMoveUnsafe, runMoveUnsafe, moveDeclsAndClean, moveDecls) where

import Control.Exception (SomeException)
import Control.Lens (makeLenses, view)
import Control.Monad (foldM, void, when)
import Control.Monad.RWS (modify, MonadWriter(tell))
import Control.Monad.State (execState, MonadState)
import Data.Default (def)
import Data.Foldable as Foldable (find)
import Data.Generics (Data)
import Data.List ((\\), foldl', intercalate, nub, stripPrefix)
import Data.Map as Map (insertWith, Map, mapWithKey, toList)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set, toList)
import Debug.Trace (trace)
import GHC (GHCOpts(hsSourceDirs))
import Graph
import Imports (cleanImports)
import qualified Language.Haskell.Exts.Annotated as A (Annotated(ann), Decl(TypeSig), ExportSpec, ExportSpecList(ExportSpecList), ImportDecl(importModule, importSpecs), ImportSpec, ImportSpecList(ImportSpecList), Module(Module), ModuleHead(ModuleHead), ModuleName(..), ModulePragma, Name, SrcInfo)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec, sModuleName, sModulePragma, sName)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(..), ImportDecl(..), ImportSpec(IThingAll, IThingWith, IVar), ModuleName(..), ModulePragma(..), Name(..), QName(Qual, Special, UnQual))
import Language.Haskell.Names (Environment, resolve, symbolName)
import LoadModule (Annot, loadModule, loadModules)
import ModuleInfo (getTopDeclSymbols', ModuleInfo(..))
import ModuleKey (moduleFullPath, ModuleKey(..), moduleName)
import MoveSpec (applyMoveSpec, MoveSpec)
import Names (topDeclExportSpec)
import SrcLoc (EndLoc(endLoc), endOfHeader, endOfImports, keep, keepAll, ScanM, scanModule, skip, srcLoc, startOfImports, withTrailingWhitespace)
import System.FilePath.Find as FilePath ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import Utils (EZPrint(ezPrint), gFind, listPairs, prettyPrint', replaceFile, simplify, withCleanRepo, withCurrentDirectory, withTempDirectory)

$(makeLenses ''Rd)

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

moveDeclsAndClean :: MoveSpec -> FilePath -> [FilePath] -> [ModuleInfo Annot] -> IO ()
moveDeclsAndClean mv scratch hsSourceDirs mods = do
  -- Move the declarations and rewrite the updated modules
  let env = resolve (map _module mods) mempty
      -- ann = map (annotate env) (map _module mods)
      rd = (Rd mods env)
  oldPaths <-
      mapM (\(m, s) -> do
              let p = _modulePath m
              case _moduleText m == s of
                True -> return Nothing
                False -> replaceFile p s >> return (Just p))
           (zip mods (moveDecls rd mv))
  newPaths <- mapM (\(k, s) -> do
                      let path = moduleFullPath k
                      void $ replaceFile (trace ("New file: " ++ show path) path) s
                      pure path)
                   (Map.toList (newModuleMap rd mv))
  -- Re-read the updated modules and clean their imports
  -- (Later we will need to find the newly created modules here)
  modules' <- mapM (\p -> either (loadError p) id <$> loadModule def p) (catMaybes oldPaths ++ newPaths) :: IO [ModuleInfo SrcSpanInfo]
  cleanImports scratch (def {hsSourceDirs = hsSourceDirs}) modules'
    where
      loadError :: FilePath -> SomeException -> ModuleInfo SrcSpanInfo
      loadError p e = error ("Unable to load updated module " ++ show p ++ ": " ++ show e)
      -- t1 rd@(Rd _ _ env) = trace ("environment: " ++ show env) rd

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: (A.SrcInfo l, Data l, EndLoc l, Eq l) => Show l => Rd l -> MoveSpec -> [String]
moveDecls rd mv =
    map (\info -> moveDeclsOfModule rd mv info) (view modules rd)

-- | Update one module and return its text
moveDeclsOfModule :: (A.SrcInfo l, Show l, Data l, EndLoc l, Eq l) => Rd l -> MoveSpec -> ModuleInfo l -> String
moveDeclsOfModule rd mv info@(ModuleInfo {_module = A.Module l _ ps _ _}) =
    scanModule (do keep (srcLoc l)
                   tell (newPragmas rd mv (_moduleKey info) ps)
                   updateHeader rd mv info
                   updateImports rd mv info
                   updateDecls rd mv info
                   keepAll)
               info
moveDeclsOfModule _ _ x = error $ "moveDeclsOfModule - unexpected module: " ++ show (_module x)

exportSep :: String
exportSep = "\n    , "

-- | Build the new modules
newModuleMap :: forall l. (A.SrcInfo l, EndLoc l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> Map ModuleKey String
newModuleMap rd@(Rd mods _env) mv =
    textMap
    where
      textMap :: Map ModuleKey String
      textMap = Map.mapWithKey (\k ds -> doModule k ds) declMap
          where
            doModule :: ModuleKey -> [(ModuleKey, A.Decl l)] -> String
            -- Build module thiskey from the list of (fromkey, decl) pairs
            doModule thisKey pairs =
                -- let thisMod = findModuleByKeyUnsafe mods thisKey in
                newPragmas rd mv thisKey [] ++
                "module " ++ maybe "Main" prettyPrint (moduleName thisKey) ++ "(" ++
                intercalate exportSep (newExports mv mods thisKey) ++ "\n    ) where\n\n" ++
                concatMap (\(someKey, _ds) -> importsForArrivingDecls mv thisKey [] (findModuleByKeyUnsafe mods someKey)) pairs ++
                -- importsForDepartingDecls mv mods thisKey -- new module, no decls can depart
                newDecls mv mods thisKey
      declMap :: Map ModuleKey [(ModuleKey, A.Decl l)]
      declMap = foldl' doModule mempty mods
          where
            doModule :: Map ModuleKey [(ModuleKey, A.Decl l)] -> ModuleInfo l -> Map ModuleKey [(ModuleKey, A.Decl l)]
            doModule mp i@(ModuleInfo {_module = A.Module _ _ _ _ ds}) = foldl' (doDecl i) mp ds
            doModule mp _ = mp
            doDecl i mp d = let k' = applyMoveSpec mv i d in
                            if Set.member k' oldKeys then mp else Map.insertWith (++) k' [(_moduleKey i, d)] mp
      oldKeys :: Set ModuleKey
      oldKeys = Set.fromList (map _moduleKey mods)

-- If a declaration is arriving in this module we need to add all the LANGUAGE
-- pragmas from that module to this one.
newPragmas :: forall l. (A.SrcInfo l, Data l) => Rd l -> MoveSpec -> ModuleKey -> [A.ModulePragma l] -> String
newPragmas (Rd mods _env) mv thisKey thesePragmas =
  let (arriving :: Set S.ModulePragma) =
          execState (mapM_ (\someMod@(ModuleInfo {_moduleKey = someKey,_module = (A.Module _ _mh somePragmas _is someDecls)}) ->
                                when
                                  (someKey /= thisKey)
                                  (mapM_ (\d -> when
                                                  (applyMoveSpec mv someMod d == thisKey)
                                                  (mapM_ addPragma (pragmaDiff somePragmas thesePragmas))) someDecls)) mods) mempty in
  unlines (map prettyPrint' (Set.toList arriving))
    where
      addPragma :: MonadState (Set S.ModulePragma) m => S.Name -> m ()
      addPragma name = modify (Set.insert (S.LanguagePragma (SrcLoc "" 1 1) [name]))
      pragmaDiff :: [A.ModulePragma l] -> [A.ModulePragma l] -> [S.Name]
      pragmaDiff ps qs = concatMap pragmaLanguageNames (map sModulePragma ps) \\ concatMap pragmaLanguageNames (map sModulePragma qs)
      pragmaLanguageNames :: S.ModulePragma -> [S.Name]
      pragmaLanguageNames (S.LanguagePragma _l names) = names
      pragmaLanguageNames _ = []

-- | Write the new export list.  Exports of symbols that have moved
-- out are removed.  Exports of symbols that have moved in are added
-- *if* the symbol is imported anywhere else.
updateHeader :: forall l. (A.SrcInfo l, Data l, EndLoc l, Show l) => Rd l -> MoveSpec -> ModuleInfo l -> ScanM ()
updateHeader (Rd _mods _env) mv
             i@(ModuleInfo {_moduleKey = k,
                            _module = m@(A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ specs)))) _ _ _)}) = do
  maybe (pure ()) (keep . srcLoc . A.ann) (listToMaybe specs)
  foldM doExport False specs
  keep (endOfHeader m)
  withTrailingWhitespace keep (startOfImports m)
    where
      doExport :: Bool -> A.ExportSpec l -> ScanM Bool
      doExport needSep spec =
          case findNewKeyOfExportSpec mv i spec of
            Just k' | k' /= k -> skip (endLoc (A.ann spec)) >> pure needSep
            _ | needSep -> keep (endLoc (A.ann spec)) >> pure True
            _ -> skip (srcLoc (A.ann spec)) >> keep (endLoc (A.ann spec)) >> pure True
updateHeader (Rd _mods _env) mv
             i@(ModuleInfo {_moduleKey = k,
                            _module = m@(A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _)}) =
  do keep (endOfHeader m)
     withTrailingWhitespace keep (startOfImports m)
updateHeader _ _ _ = pure ()

-- | Text of exports added due to arriving declarations
newExports :: forall l. (A.SrcInfo l, Data l, Eq l, Show l) => MoveSpec -> [ModuleInfo l] -> ModuleKey -> [String]
newExports mv mods thisKey =
    map prettyPrint names
    where
      -- (This should be inferred from the text of thisKey.)
      names = nub $ concatMap newExportsFromModule (filter (\x -> _moduleKey x /= thisKey) mods)
      -- Scan a module other than thisKey for declarations moving to thisKey.  If
      -- found, transfer the export from there to here.
      newExportsFromModule :: ModuleInfo l -> [S.ExportSpec]
      newExportsFromModule i'@(ModuleInfo {_module = A.Module _ _ _ _ ds}) =
          let i = simplify i' in
          mapMaybe (\(d :: A.Decl ()) -> if (applyMoveSpec mv i (d :: A.Decl ()) == thisKey) then topDeclExportSpec i d else Nothing) (map simplify ds)
      -- We can't import from a module without an explicit name in its header
      newExportsFromModule (ModuleInfo {_moduleKey = ModuleFullPath {}}) = []
      newExportsFromModule x = error $ "newExports - unexpected module: " ++ show (_module x)

findNewKeyOfExportSpec :: (A.SrcInfo l, Data l, Show l) => MoveSpec -> ModuleInfo l -> A.ExportSpec l -> Maybe ModuleKey
findNewKeyOfExportSpec mv mi espec = fmap (applyMoveSpec mv mi) (findDeclOfExportSpec mi espec)

-- | Find the declaration that causes all the symbols in the
-- ExportSpec to come into existance.
findDeclOfExportSpec :: forall l. (A.SrcInfo l, Data l, Show l) => ModuleInfo l -> A.ExportSpec l -> Maybe (A.Decl l)
findDeclOfExportSpec mi spec =
    findDeclOfSymbols mi (Set.fromList (map sName (gFind spec :: [A.Name l])))
    where
      findDeclOfSymbols :: ModuleInfo l -> Set S.Name -> Maybe (A.Decl l)
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ _}) syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ decls}) syms =
          case filter (isSubsetOf syms . symset) (filter notSig decls) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map (srcLoc . A.ann) ds)
      findDeclOfSymbols x _ = error $ "findDeclOfExportSpec - unexpected module: " ++ show (_module x)
      symset = Set.fromList . map symbolName . getTopDeclSymbols' mi

-- Find the declaration of the symbols of an import spec.  If that
-- declaration moved, update the module name.
updateImports :: forall l. (A.SrcInfo l, EndLoc l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> ModuleInfo l -> ScanM ()
updateImports rd@(Rd mods _env) mv (ModuleInfo {_moduleKey = thisKey, _module = (A.Module _ _ _ thisModuleImports _)}) = do
  -- Update the existing imports
  mapM_ (uncurry doImportDecl) (listPairs thisModuleImports)
  mapM_ doNewImports (filter (\m' -> _moduleKey m' /= thisKey) mods)
  tell $ importsForDepartingDecls rd mv (findModuleByKey mods thisKey)
    where
      -- Process one import declaration.  Each of its specs will
      -- either be kept, discarded, or moved to a new import with a
      -- new module name.
      doImportDecl :: Maybe (A.ImportDecl l) -> Maybe (A.ImportDecl l) -> ScanM ()
      doImportDecl Nothing Nothing = pure ()
      doImportDecl Nothing (Just first) = keep (srcLoc (A.ann first))
      doImportDecl (Just x) next = do
        let iname = simplify (A.importModule x)
        case A.importSpecs x of
          Nothing -> do
            keep (endLoc (A.ann x))
            withTrailingWhitespace keep (fmap (srcLoc . A.ann) next)
          Just (A.ImportSpecList l hiding specs) -> do
            let dests = map (newModuleOfImportSpec rd mv iname) specs
            case not (null specs) && all (/= (Just iname)) dests of
              -- Discard the import if it becomes empty, but not if
              -- it was empty to begin with.
              True ->
                withTrailingWhitespace skip (fmap (srcLoc . A.ann) next)
              False -> do
                keep $ case specs of
                         (spec : _) -> srcLoc (A.ann spec)
                         [] -> srcLoc l
                mapM_ (uncurry (doImportSpec iname hiding)) (listPairs specs)
                keep (endLoc l)
                withTrailingWhitespace keep (fmap (srcLoc . A.ann) next)
            -- If declarations move from "here" to "there", import
            -- "there" in case declarations remaining "here" still use
            -- declarations now in "there".
            mapM_ (importsForMovingDecls iname hiding) specs

      doImportSpec :: A.ModuleName () -> Bool -> Maybe (A.ImportSpec l) -> Maybe (A.ImportSpec l) -> ScanM ()
      doImportSpec _ _ Nothing _ = pure ()
      doImportSpec name hiding (Just spec) next =
          case newModuleOfImportSpec rd mv name spec of
            Just name'
                | -- Retain import if module name is unchanged
                  name' == name -> keep nextLoc
            _ -> skip nextLoc
          where
            nextLoc = maybe (endLoc (A.ann spec)) (srcLoc . A.ann) next

      importsForMovingDecls :: A.ModuleName () -> Bool -> A.ImportSpec l -> ScanM ()
      importsForMovingDecls name hiding spec =
          case newModuleOfImportSpec rd mv name spec of
            Just name'
                | -- has import module name changed?
                  name' /= name &&
                  -- don't generate self imports
                  name' /= thisModuleName ->
                    tell ("\nimport " ++ prettyPrint' name' ++ " (" ++ prettyPrint' spec ++ ")" ++ "\n")
            _ -> pure ()

      thisModuleName = case thisKey of
                         ModuleKey {_moduleName = x} -> x
                         _ -> A.ModuleName () "Main"
      -- Add new imports due to declarations moving from someKey to
      -- thisKey.  All of the imports in someKey must be duplicated in
      -- thisKey (except for imports of thisKey).  Also, all of the
      -- exports in someKey must be turned into imports to thisKey
      -- (with updated module names.)  Finally, if a declaration
      -- moves from thisKey to someKey, we need to add an import of it
      -- here (as long as someKey doesn't import thisKey)
      doNewImports :: ModuleInfo l -> ScanM ()
      doNewImports someModule = do
        tell $ importsForArrivingDecls mv thisKey thisModuleImports someModule
updateImports _ _ x = error $ "updateImports - unexpected module: " ++ show (_module x)

-- | If a decl is move from thisModule to someModule we may need to
-- import its symbols to satisfy remaining declarations that use it.
-- However, if someModule already imports thisModule that will cause
-- an import cycle.  Unfortunately, even if someModule *does not* import
-- thisModule a cycle can appear, because of the code that converts the
-- exports of someModule into imports in thisModule.
importsForDepartingDecls :: (A.SrcInfo l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> Maybe (ModuleInfo l) -> String
importsForDepartingDecls rd@(Rd mods _env) mv (Just thisMod@(ModuleInfo {_moduleKey = thisKey@(ModuleKey {_moduleName = thisModuleName}), _module = A.Module _ _ _ _ ds})) =
    concatMap (\d -> case applyMoveSpec mv thisMod d of
                       someKey@(ModuleKey {_moduleName = someModuleName})
                           | someKey /= thisKey ->
                               case t2 d someKey (findModuleByKey mods someKey) of
                                 Just (ModuleInfo {_module = A.Module _ _ _ someModuleImports _}) ->
                                     case moveType someModuleImports thisModuleName of
                                       Down -> maybe "" (\i -> prettyPrint' i ++ "\n") (importSpecFromDecl thisMod someModuleName d)
                                       Up -> ""
                                 -- Moving a declaration from thisKey to someKey is a "Down"
                                 -- move if there are any remaining uses of those symbols in
                                 -- thisKey.  It is an "Up" move if the declarations still
                                 -- depend on exports of thisKey.  FIXME: I haven't
                                 -- implemented theses tests yet, so assume Down for now.
                                 Just _ -> error "Unexpected module"
                                 Nothing -> maybe "" (\i -> prettyPrint' i ++ "\n") (importSpecFromDecl thisMod someModuleName d)
                       _ -> "") ds
    where
      t2 d someKey x =
          trace ("import departing: " ++ ezPrint (thisMod, d) ++ " from " ++ ezPrint thisKey ++ " -> " ++ ezPrint someKey ++ ", moveType=" ++ moveType' rd thisKey someKey) x

importsForDepartingDecls _ _ _ = ""

-- | Copy the entire import list of the departure module - it will be
-- cleaned up later.  Also, if this is an Up move we can import the
-- departure module itself.  If it is a down move the departure module
-- will be importing the arriving declaration.
importsForArrivingDecls :: (A.SrcInfo l, Data l, Show l) => MoveSpec -> ModuleKey -> [A.ImportDecl l] -> ModuleInfo l -> String
importsForArrivingDecls mv thisKey thisModuleImports someMod@(ModuleInfo {_moduleKey = someKey, _module = A.Module _ _ _ someModuleImports ds}) =
    if any (\d -> applyMoveSpec mv someMod d == thisKey) ds
    then concatMap
             (\i -> prettyPrint' i ++ "\n")
             (filter (\i -> moduleName thisKey /= Just (simplify (A.importModule i))) someModuleImports) ++
         concatMap
             (\i -> prettyPrint' i ++ "\n")
             (maybeToList (importDeclFromExportSpecs mv thisModuleImports someMod)) ++
         case (fmap (moveType someModuleImports) (moduleName thisKey), moduleName someKey) of
           (Just Up, Just someName) -> "import " ++ prettyPrint someName ++ "\n"
           _ -> ""
    else ""
importsForArrivingDecls _ _ _ _ = ""

-- | If a declaration moves from someModule to thisModule, and nothing
-- in thisModule is imported by someModule, add imports to thisModule
-- of all the symbols exported by someModule.
importDeclFromExportSpecs :: (A.SrcInfo l, Data l, Show l) => MoveSpec -> [A.ImportDecl l] -> ModuleInfo l -> Maybe S.ImportDecl
importDeclFromExportSpecs moveSpec
                          thisModuleImports
                          someInfo@(ModuleInfo
                                    {_moduleKey = someKey@(ModuleKey {_moduleName = someModuleName}),
                                     _module = someModule@(A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ especs@(_ : _))))) _ _ _)}) =
    -- Do we need to import the remaining exports from the departure
    -- module into the arrival module?  Only if we are moving the
    -- declaration 'Up', which implies that it may use symbols from
    -- the departure module.
    case moveType thisModuleImports someModuleName of
      Up -> Just (S.ImportDecl { S.importLoc = srcLoc (A.ann someModule)
                               , S.importModule = maybe (S.ModuleName "Main") id (fmap sModuleName (moduleName someKey))
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
          map exportToImport' (filter (\e -> findNewKeyOfExportSpec moveSpec someInfo e == Just (_moduleKey someInfo)) especs)
          where
importDeclFromExportSpecs _ _ _ = Nothing

-- | These cases probably need work.
exportToImport' :: A.ExportSpec l -> S.ImportSpec
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

-- | Build an ImportDecl that imports the symbols of d from m.
importSpecFromDecl :: forall l. (A.SrcInfo l, Eq l, Data l, Show l) => ModuleInfo l -> A.ModuleName () -> A.Decl l -> Maybe S.ImportDecl
importSpecFromDecl thisMod newModName d =
    case map exportToImport (maybeToList (topDeclExportSpec (simplify thisMod) (simplify d))) of
      [] -> Nothing
      imports -> Just (S.ImportDecl { S.importLoc = srcLoc (A.ann d)
                                    , S.importModule = sModuleName newModName
                                    , S.importQualified = False
                                    , S.importSrc = False
                                    , S.importSafe = False
                                    , S.importPkg = Nothing
                                    , S.importAs = Nothing
                                    , S.importSpecs = Just (False, imports) })

-- | Given in import spec and the name of the module it was imported
-- from, return the name of the new module where it will now be
-- imported from.
newModuleOfImportSpec :: (A.SrcInfo l, Data l, Show l) => Rd l -> MoveSpec -> A.ModuleName () -> A.ImportSpec l -> Maybe (A.ModuleName ())
newModuleOfImportSpec (Rd mods _env) mv oldModname spec =
    case findModuleByName mods oldModname of
      Just info -> case findDeclOfImportSpec info spec of
                     Just d -> moduleName (applyMoveSpec mv info d)
                     -- Everything else we can leave alone - even if we can't
                     -- find a declaration, they might be re-exported.
                     Nothing {- | isReexport info spec -} -> Just oldModname
                     -- Nothing -> trace ("Unable to find decl of " ++ prettyPrint' spec ++ " in " ++ show oldModname) Nothing
      -- If we don't know about the module leave the import spec alone
      Nothing -> Just oldModname

findModuleByName :: forall l. (Show l) => [ModuleInfo l] -> A.ModuleName () -> Maybe (ModuleInfo l)
findModuleByName mods oldModname =
    case filter (testModuleName oldModname) mods of
      [m] -> Just m
      [] -> Nothing
      _ms -> error $ "Multiple " ++ show oldModname
    where
      testModuleName :: A.ModuleName () -> ModuleInfo l -> Bool
      testModuleName modName (ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ name _ _)) _ _ _}) =
          simplify name == modName
      testModuleName modName (ModuleInfo {_module = A.Module _ Nothing _ _ _}) =
          modName == A.ModuleName () "Main"
      testModuleName _ x = error $ "findModuleByName - unexpected module: " ++ show (_module x)

-- | Find the declaration in a module that causes all the symbols in
-- the ImportSpec to come into existance.
findDeclOfImportSpec :: forall l. (A.SrcInfo l, Data l, Show l) => ModuleInfo l -> A.ImportSpec l -> Maybe (A.Decl l)
findDeclOfImportSpec info spec = findDeclOfSymbols info (Set.fromList (map sName (gFind spec :: [A.Name l])))
    where
      findDeclOfSymbols :: ModuleInfo l -> Set S.Name -> Maybe (A.Decl l)
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ _}) syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ decls}) syms =
          case filter (isSubsetOf syms . symset) (filter notSig decls) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map (srcLoc . A.ann) ds)
      findDeclOfSymbols x _ = error $ "findDeclOfImportSpec - unexpected module: " ++ show (_module x)
      symset = Set.fromList . map symbolName . getTopDeclSymbols' info

notSig :: A.Decl t -> Bool
notSig (A.TypeSig {}) = False
notSig _ = True

#if 0
-- Are each of the symbols of this import spec re-exported by
-- some export spec info?
isReexport :: ModuleInfo -> A.ImportSpec SrcSpanInfo -> Bool
isReexport info@(ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ especs)))) _ _ _}) ispec =
    let syms = Set.fromList (gFind ispec :: [A.name SrcSpanInfo]) in
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
reexports sym e = Set.member sym (Set.fromList (gFind e :: [A.Name SrcSpanInfo]))
#endif

-- | Look through a module's imports, using findDeclOfImportSpec and
-- moveSpec to determine which refer to symbols that are moving from
-- one module to another.  There are three cases for an import that
-- moves.  It might move from another module to this module, in which
-- case it can be removed.  It might move between two modules other
-- than this one, in which case the a new import with the new module
-- name is added.  The final case is invalid - a module that imported
-- itself.
updateDecls :: (Data l, A.SrcInfo l, EndLoc l, Show l) => Rd l -> MoveSpec -> ModuleInfo l -> ScanM ()
updateDecls (Rd mods _env) mv thisMod@(ModuleInfo {_module = (A.Module _ _ _ _ decls), _moduleKey = thisKey}) = do
  -- keep (endOfImports m)
  -- Declarations that were already here and are to remain
  mapM_ doDecl (listPairs decls)
  keepAll
  tell $ newDecls mv mods thisKey
    where
      doDecl (Nothing, Nothing) = pure ()
      doDecl (Nothing, Just _first) = pure () -- keep (srcLoc first)
      doDecl (Just d, next) =
          case applyMoveSpec mv thisMod d of
            someKey | someKey /= thisKey -> do
              trace ("decl departing: " ++ ezPrint (thisMod, d) ++ " from " ++ ezPrint thisKey ++ " to " ++ ezPrint someKey) (pure ())
              skip (endLoc (A.ann d))
              withTrailingWhitespace skip (fmap (srcLoc . A.ann) next)
            _ -> do
              keep (endLoc (A.ann d))
              withTrailingWhitespace keep (fmap (srcLoc . A.ann) next)
updateDecls _ _ x = error $ "updateDecls - unexpected module: " ++ show (_module x)

-- | Declarations that are moving here from other modules.
newDecls :: forall l. (A.SrcInfo l, EndLoc l, Data l) => MoveSpec -> [ModuleInfo l] -> ModuleKey -> String
newDecls mv mods thisKey =
    concatMap doModule mods
    where
      -- Scan the declarations of all the modules except this one
      doModule someMod@(ModuleInfo {_module = (A.Module _l _mh _ps _is decls),
                                    _moduleKey = someKey})
          | someKey /= thisKey =
              scanModule (do skip (endOfImports (_module someMod))
                             mapM_ (uncurry (doDecl someMod)) (listPairs decls))
                         someMod
      doModule _ = ""

      -- If a declaration is to be moved to this module, extract its
      -- text and add it to the result.
      doDecl :: ModuleInfo l -> Maybe (A.Decl l) -> Maybe (A.Decl l) -> ScanM ()
      doDecl _ Nothing _ = pure ()
      doDecl someMod (Just d) next =
          case applyMoveSpec mv someMod d == thisKey of
            False -> do
              skip (endLoc (A.ann d))
              withTrailingWhitespace skip (fmap (srcLoc . A.ann) next)
            True -> do
              keep (endLoc (A.ann d))
              withTrailingWhitespace keep (fmap (srcLoc . A.ann) next)

{-
-- | Get the text of a declaration including the preceding whitespace
declText :: ModuleInfo SrcSpanInfo -> A.Decl SrcSpanInfo -> String
declText (ModuleInfo {_module = m@(A.Module _ mh ps is ds), _moduleText = mtext}) d =
    -- Find the end of the last object preceding d - could be the
    -- preceding declaration, the last import, the last pragma, or the
    -- module header.  If none of that exists use the module start
    -- location.
    let p = case ds of
              (d1 : _) | d == d1 -> endLoc (last (maybe [] (\x -> [A.ann x]) mh ++ map A.ann ps ++ map A.ann is))
              _ -> case dropWhile2 (\_  md2 -> Just d /= md2) ds of
                     (d1 : _) -> endLoc (A.ann d1)
                     [] -> srcLoc (A.ann m) in
    textOfSpan (mkSrcSpan p (endLoc (A.ann d))) mtext
declText x _ = error $ "declText - unexpected module: " ++ show (_module x)
-}
