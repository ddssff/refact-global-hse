{-# LANGUAGE CPP, FlexibleContexts, PackageImports, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}
module Decls
    ( environment, importGraph
    , runSimpleMove
    , runSimpleMoveUnsafe
    , runMoveUnsafe
    , moveDeclsAndClean
    , moveDecls
    ) where

import Clean (cleanImports)
import CPP (GHCOpts, hsSourceDirs)
import Control.Lens (makeLenses, set, view)
import Control.Monad (foldM, void, when)
import Control.Monad.RWS (modify, MonadWriter(tell))
import Control.Monad.State (execState, MonadState)
import Data.Default (def)
import Data.Generics (Data)
import Data.List ((\\), foldl', intercalate, nub, stripPrefix)
import Data.Map.Strict as Map (insertWith, Map, mapWithKey, toList)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set, toList)
import Debug.Trace (trace)
import Graph (findModuleByKey, findModuleByKeyUnsafe, makeImportGraph, moveType, MoveType(Down, Up), Rd(Rd))
import Language.Haskell.Exts.Syntax (Annotated(ann), Decl(TypeSig), EWildcard(..), ExportSpec, ExportSpecList(ExportSpecList),
                                                             ImportDecl(..), ImportSpec, ImportSpecList(ImportSpecList), Module(Module),
                                                             ModuleHead(ModuleHead), ModuleName(..), ModulePragma, Name, ExportSpec(..),
                                                             ImportDecl(..), ImportSpec(IThingAll, IThingWith, IVar), ModuleName(..),
                                                             ModulePragma(..), Name(..), QName(Qual, Special, UnQual))
-- import Language.Haskell.Exts.Annotated.Simplify (sExportSpec, sModuleName, sModulePragma, sName)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcInfo, SrcLoc(..), SrcSpanInfo(..))
import Language.Haskell.Names (resolve, Symbol(symbolName))
import Language.Haskell.Names.SyntaxUtils (dropAnn, getImports, getModuleDecls)
import LoadModule (Annot, loadModule, loadModules)
import ModuleInfo (getTopDeclSymbols', ModuleInfo(..))
import ModuleKey (moduleFullPath, ModuleKey(..), moduleName)
import MoveSpec (applyMoveSpec, MoveSpec)
import Names (topDeclExportSpec)
import SrcLoc (EndLoc(endLoc), endOfHeader, endOfImports, endOfImportSpecs, endOfModule, keep, keepAll, ScanM, scanModule, skip, srcLoc, startOfDecls, startOfImports, withTrailingWhitespace)
import System.FilePath.Find as FilePath ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import Utils (EZPrint(ezPrint), gFind, prettyPrint', replaceFile, withCleanRepo, withCurrentDirectory)

$(makeLenses ''Rd)

-- | Run moveDeclsAndClean on all the .hs files in the given
-- directory.
runSimpleMove :: FilePath -> MoveSpec -> IO ()
runSimpleMove top spec = withCleanRepo $ runSimpleMoveUnsafe top spec

runSimpleMoveUnsafe :: FilePath -> MoveSpec -> IO ()
runSimpleMoveUnsafe top mv = runMoveUnsafe top (set hsSourceDirs ["."] def) mv

runMoveUnsafe :: FilePath -> GHCOpts -> MoveSpec -> IO ()
runMoveUnsafe top opts mv =
    withCurrentDirectory top $ do
      paths <- (catMaybes . map (stripPrefix "./"))
               <$> (FilePath.find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
      loadModules def paths >>= moveDeclsAndClean mv opts

moveDeclsAndClean :: MoveSpec -> GHCOpts -> [ModuleInfo Annot] -> IO ()
moveDeclsAndClean mv opts mods = do
  -- Move the declarations and rewrite the updated modules
  let env = resolve (map _module mods) mempty
      -- ann = map (annotate env) (map _module mods)
      gr = makeImportGraph mods
      rd = (Rd mods env gr)
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
  modules' <- mapM (loadModule opts) (catMaybes oldPaths ++ newPaths) :: IO [ModuleInfo SrcSpanInfo]
  cleanImports [opts] modules'

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: (SrcInfo l, Data l, EndLoc l, Eq l, EZPrint l) => Show l => Rd l -> MoveSpec -> [String]
moveDecls rd mv =
    map (\info -> moveDeclsOfModule rd mv info) (view modules rd)

-- | Update one module and return its text
moveDeclsOfModule :: (SrcInfo l, Show l, Data l, EndLoc l, Eq l, EZPrint l) => Rd l -> MoveSpec -> ModuleInfo l -> String
moveDeclsOfModule rd mv info@(ModuleInfo {_module = Module l _ ps _ _}) =
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
newModuleMap :: forall l. (SrcInfo l, EndLoc l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> Map ModuleKey String
newModuleMap rd@(Rd mods _env _gr) mv =
    textMap
    where
      textMap :: Map ModuleKey String
      textMap = Map.mapWithKey (\k ds -> doModule k ds) declMap
          where
            doModule :: ModuleKey -> [(ModuleKey, Decl l)] -> String
            -- Build module thiskey from the list of (fromkey, decl) pairs
            doModule thisKey pairs =
                -- let thisMod = findModuleByKeyUnsafe mods thisKey in
                newPragmas rd mv thisKey [] ++
                "module " ++ maybe "Main" prettyPrint (moduleName thisKey) ++ "(" ++
                intercalate exportSep (newExports rd mv thisKey) ++ "\n    ) where\n\n" ++
                concatMap (\(someKey, _ds) -> importsForArrivingDecls rd mv thisKey (findModuleByKeyUnsafe mods someKey)) pairs ++
                -- importsForDepartingDecls mv mods thisKey -- new module, no decls can depart
                newDecls mv mods thisKey
      declMap :: Map ModuleKey [(ModuleKey, Decl l)]
      declMap = foldl' doModule mempty mods
          where
            doModule :: Map ModuleKey [(ModuleKey, Decl l)] -> ModuleInfo l -> Map ModuleKey [(ModuleKey, Decl l)]
            doModule mp i@(ModuleInfo {_module = m}) = foldl' (doDecl i) mp (getModuleDecls m)
            doDecl i mp d = let k' = applyMoveSpec mv i d in
                            if Set.member k' oldKeys then mp else Map.insertWith (++) k' [(_moduleKey i, d)] mp
      oldKeys :: Set ModuleKey
      oldKeys = Set.fromList (map _moduleKey mods)

-- If a declaration is arriving in this module we need to add all the LANGUAGE
-- pragmas from that module to this one.
newPragmas :: forall l. (SrcInfo l, Data l) => Rd l -> MoveSpec -> ModuleKey -> [ModulePragma l] -> String
newPragmas (Rd mods _env _gr) mv thisKey thesePragmas =
  let (arriving :: Set (ModulePragma ())) =
          execState (mapM_ (\someMod@(ModuleInfo {_moduleKey = someKey,_module = (Module _ _mh somePragmas _is someDecls)}) ->
                                when
                                  (someKey /= thisKey)
                                  (mapM_ (\d -> when
                                                  (applyMoveSpec mv someMod d == thisKey)
                                                  (mapM_ addPragma (pragmaDiff somePragmas thesePragmas))) someDecls)) mods) mempty in
  unlines (map prettyPrint' (Set.toList arriving))
    where
      addPragma :: MonadState (Set (ModulePragma ())) m => Name () -> m ()
      addPragma name = modify (Set.insert (LanguagePragma () [name]))
      pragmaDiff :: [ModulePragma l] -> [ModulePragma l] -> [Name ()]
      pragmaDiff ps qs = concatMap pragmaLanguageNames (map dropAnn ps) \\ concatMap pragmaLanguageNames (map dropAnn qs)
      pragmaLanguageNames :: ModulePragma () -> [Name ()]
      pragmaLanguageNames (LanguagePragma _l names) = names
      pragmaLanguageNames _ = []

-- | Write the new export list.  Exports of symbols that have moved
-- out are removed.  Exports of symbols that have moved in are added
-- *if* the symbol is imported anywhere else.
updateHeader :: forall l. (SrcInfo l, Data l, EndLoc l, Eq l, Show l) => Rd l -> MoveSpec -> ModuleInfo l -> ScanM ()
updateHeader rd@(Rd _mods _env _gr) mv
             mi@(ModuleInfo {_moduleKey = k,
                             _module = m@(Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ specs)))) _ _ _)}) = do
  maybe (pure ()) (keep . srcLoc . ann) (listToMaybe specs)
  needSep <- foldM doExport False specs
  case newExports rd mv k of
    [] -> pure ()
    xs -> do when needSep (tell exportSep)
             tell (intercalate exportSep xs)
  keep (endOfHeader m)
  withTrailingWhitespace keep (startOfImports mi)
    where
      doExport :: Bool -> ExportSpec l -> ScanM Bool
      doExport needSep spec =
          case findNewKeyOfExportSpec mv mi spec of
            Just k' | k' /= k -> skip (endLoc (ann spec)) >> pure needSep
            _ | needSep -> keep (endLoc (ann spec)) >> pure True
            _ -> skip (srcLoc (ann spec)) >> keep (endLoc (ann spec)) >> pure True
updateHeader (Rd _mods _env _gr) _mv
             mi@(ModuleInfo {_module = m@(Module _ (Just (ModuleHead _ _ _ Nothing)) _ _ _)}) =
  do keep (endOfHeader m)
     withTrailingWhitespace keep (startOfImports mi)
updateHeader _ _ _ = pure ()

-- | Text of exports added due to arriving declarations
newExports :: forall l. (SrcInfo l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> ModuleKey -> [String]
newExports (Rd mods _ _) mv thisKey =
    map prettyPrint names
    where
      -- (This should be inferred from the text of thisKey.)
      names = nub $ concatMap newExportsFromModule (filter (\x -> _moduleKey x /= thisKey) mods)
      -- Scan a module other than thisKey for declarations moving to thisKey.  If
      -- found, transfer the export from there to here.
      newExportsFromModule :: ModuleInfo l -> [ExportSpec ()]
      newExportsFromModule i'@(ModuleInfo {_moduleKey = ModuleKey {}, _module = m}) =
          let i = dropAnn i' in
          mapMaybe (\(d :: Decl ()) -> if (applyMoveSpec mv i (d :: Decl ()) == thisKey) then topDeclExportSpec i d else Nothing)
                   (map dropAnn (getModuleDecls m))
      -- We can't import from a module without an explicit name in its header
      newExportsFromModule (ModuleInfo {_moduleKey = ModuleFullPath {}}) = []

findNewKeyOfExportSpec :: (SrcInfo l, Data l, Show l) => MoveSpec -> ModuleInfo l -> ExportSpec l -> Maybe ModuleKey
findNewKeyOfExportSpec mv mi espec = fmap (applyMoveSpec mv mi) (findDeclOfExportSpec mi espec)

-- | Find the declaration that causes all the symbols in the
-- ExportSpec to come into existance.
findDeclOfExportSpec :: forall l. (SrcInfo l, Data l, Show l) => ModuleInfo l -> ExportSpec l -> Maybe (Decl l)
findDeclOfExportSpec mi spec =
    findDeclOfSymbols mi (Set.fromList (map dropAnn (gFind spec :: [Name l])))
    where
      findDeclOfSymbols :: ModuleInfo l -> Set (Name ()) -> Maybe (Decl l)
      findDeclOfSymbols _ syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = m}) syms =
          case filter (isSubsetOf syms . symset) (filter notSig (getModuleDecls m)) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map (srcLoc . ann) ds)
      symset = Set.fromList . map symbolName . getTopDeclSymbols' mi

-- Find the declaration of the symbols of an import spec.  If that
-- declaration moved, update the module name.
updateImports :: forall l. (SrcInfo l, EndLoc l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> ModuleInfo l -> ScanM ()
updateImports rd@(Rd mods _env _gr) mv mi@(ModuleInfo {_moduleKey = thisKey, _module = Module _ _ _ thisModuleImports _}) = do
  -- Update the existing imports
  mapM_ (uncurry doImportDecl) (zip thisModuleImports (tail (map (srcLoc . ann) thisModuleImports ++ [startOfDecls mi])))
  mapM_ doNewImports (filter (\m' -> _moduleKey m' /= thisKey) mods)
  tell $ importsForDepartingDecls rd mv (findModuleByKey mods thisKey)
    where
      -- Process one import declaration.  Each of its specs will
      -- either be kept, discarded, or moved to a new import with a
      -- new module name.
      doImportDecl :: ImportDecl l -> SrcLoc -> ScanM ()
      doImportDecl x next = do
        let iname = dropAnn (importModule x)
        case importSpecs x of
          Nothing -> do
            keep (endLoc (ann x))
            withTrailingWhitespace keep next
          Just (ImportSpecList l hiding specs) -> do
            let dests = map (newModuleOfImportSpec rd mv iname) specs
            case not (null specs) && all (/= (Just iname)) dests of
              -- Discard the import if it becomes empty, but not if
              -- it was empty to begin with.
              True ->
                withTrailingWhitespace skip next
              False -> do
                keep $ case specs of
                         (spec : _) -> srcLoc (ann spec)
                         [] -> srcLoc l
                mapM_ (uncurry (doImportSpec iname hiding)) (zip specs (tail (map (srcLoc . ann) specs) ++ [endOfImportSpecs x]))
                keep (endLoc l)
                withTrailingWhitespace keep next
            -- If declarations move from "here" to "there", import
            -- "there" in case declarations remaining "here" still use
            -- declarations now in "there".
            mapM_ (importsForMovingDecls iname hiding) specs
      doImportSpec :: ModuleName () -> Bool -> ImportSpec l -> SrcLoc -> ScanM ()
      doImportSpec name hiding spec next =
          case newModuleOfImportSpec rd mv name spec of
            Just name'
                | -- Retain import if module name is unchanged
                  name' == name -> keep next
            _ -> skip next

      importsForMovingDecls :: ModuleName () -> Bool -> ImportSpec l -> ScanM ()
      importsForMovingDecls name hiding spec =
          case newModuleOfImportSpec rd mv name spec of
            Just name'
                | -- has import module name changed?
                  name' /= name &&
                  -- don't generate self imports
                  name' /= thisModuleName ->
                    tell ("import " ++ prettyPrint' name' ++ " (" ++ prettyPrint' spec ++ ")" ++ "\n")
            _ -> pure ()

      thisModuleName = case thisKey of
                         ModuleKey {_moduleName = x} -> x
                         _ -> ModuleName () "Main"
      -- Add new imports due to declarations moving from someKey to
      -- thisKey.  All of the imports in someKey must be duplicated in
      -- thisKey (except for imports of thisKey).  Also, all of the
      -- exports in someKey must be turned into imports to thisKey
      -- (with updated module names.)  Finally, if a declaration
      -- moves from thisKey to someKey, we need to add an import of it
      -- here (as long as someKey doesn't import thisKey)
      doNewImports :: ModuleInfo l -> ScanM ()
      doNewImports someModule = do
        tell $ importsForArrivingDecls rd mv thisKey someModule
updateImports _ _ x = error $ "updateImports - unexpected module: " ++ show (_module x)

-- | If a decl is move from thisModule to someModule we may need to
-- import its symbols to satisfy remaining declarations that use it.
-- However, if someModule already imports thisModule that will cause
-- an import cycle.  Unfortunately, even if someModule *does not* import
-- thisModule a cycle can appear, because of the code that converts the
-- exports of someModule into imports in thisModule.
importsForDepartingDecls :: (SrcInfo l, Data l, Eq l, Show l) => Rd l -> MoveSpec -> Maybe (ModuleInfo l) -> String
importsForDepartingDecls rd@(Rd mods _env _gr) mv (Just thisMod@(ModuleInfo {_moduleKey = thisKey, _module = m})) =
    concatMap (\d -> case applyMoveSpec mv thisMod d of
                       someKey@(ModuleKey {_moduleName = someModuleName})
                           | someKey /= thisKey ->
                               case t2 d someKey (findModuleByKey mods someKey) of
                                 Just (ModuleInfo {_module = Module _ _ _ _ _}) ->
                                     case moveType rd thisKey someKey of
                                       Down -> maybe "" (\i -> prettyPrint' i ++ "\n") (importSpecFromDecl thisMod someModuleName d)
                                       _ -> ""
                                 -- Moving a declaration from thisKey to someKey is a "Down"
                                 -- move if there are any remaining uses of those symbols in
                                 -- thisKey.  It is an "Up" move if the declarations still
                                 -- depend on exports of thisKey.  FIXME: I haven't
                                 -- implemented theses tests yet, so assume Down for now.
                                 Just _ -> error "Unexpected module"
                                 Nothing -> maybe "" (\i -> prettyPrint' i ++ "\n") (importSpecFromDecl thisMod someModuleName d)
                       _ -> "") (getModuleDecls m)
    where
      t2 d someKey x =
          trace ("import departing: " ++ ezPrint (thisMod, d) ++ " from " ++ ezPrint thisKey ++ " -> " ++ ezPrint someKey ++ ", moveType=" ++ show (moveType rd thisKey someKey)) x

importsForDepartingDecls _ _ _ = ""

-- | Copy the entire import list of the departure module - it will be
-- cleaned up later.  Also, if this is an Up move we can import the
-- departure module itself.  If it is a down move the departure module
-- will be importing the arriving declaration.
importsForArrivingDecls :: Rd l -> (SrcInfo l, Data l, Show l) => MoveSpec -> ModuleKey -> ModuleInfo l -> String
importsForArrivingDecls rd mv thisKey someMod@(ModuleInfo {_moduleKey = someKey, _module = m}) =
    if any (\d -> applyMoveSpec mv someMod d == thisKey) (getModuleDecls m)
    then concatMap
             (\i -> prettyPrint' i ++ "\n")
             (filter (\i -> moduleName thisKey /= Just (dropAnn (importModule i))) (getImports m)) ++
         concatMap
             (\i -> prettyPrint' i ++ "\n")
             (maybeToList (importDeclFromExportSpecs rd mv thisKey someMod)) ++
         case (moveType rd someKey thisKey, moduleName someKey) of
           (Up, Just someName) -> "import " ++ prettyPrint someName ++ "\n"
           _ -> ""
    else ""

-- | If a declaration moves from someModule to thisModule, and nothing
-- in thisModule is imported by someModule, add imports to thisModule
-- of all the symbols exported by someModule.
importDeclFromExportSpecs :: forall l. (SrcInfo l, Data l, Show l) => Rd l -> MoveSpec -> ModuleKey -> ModuleInfo l -> Maybe (ImportDecl ())
importDeclFromExportSpecs rd moveSpec
                          thisKey
                          someInfo@(ModuleInfo
                                    {_moduleKey = someKey,
                                     _module = Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ especs@(_ : _))))) _ _ _}) =
    -- Do we need to import the remaining exports from the departure
    -- module into the arrival module?  Only if we are moving the
    -- declaration 'Up', which implies that it may use symbols from
    -- the departure module.
    case moveType rd someKey thisKey of
      Up -> Just (ImportDecl { importAnn = ()
                             , importModule = maybe (ModuleName () "Main") id (fmap dropAnn (moduleName someKey))
                             , importQualified = False
                             , importSrc = False
                             , importSafe = False
                             , importPkg = Nothing
                             , importAs = Nothing
                             , importSpecs = Just importSpecsFromExportSpecs })
      Down -> Nothing
    where
      -- Build ImportSpecs of m corresponding to some export specs.
      importSpecsFromExportSpecs :: ImportSpecList ()
      importSpecsFromExportSpecs =
          ImportSpecList () False (map exportToImport' (filter (\e -> findNewKeyOfExportSpec moveSpec someInfo e == Just (_moduleKey someInfo)) especs))
          where
importDeclFromExportSpecs _ _ _ _ = Nothing

-- | These cases probably need work.
exportToImport' :: ExportSpec l -> ImportSpec ()
exportToImport' = exportToImport . dropAnn

exportToImport :: ExportSpec () -> ImportSpec ()
exportToImport (EVar () (Qual () _mname name)) = IVar () name
exportToImport (EVar () (UnQual () name)) = IVar () name
exportToImport x@(EVar () (Special () _)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(EAbs () _space (UnQual () _name)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(EAbs () _space (Qual () _mname _name)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(EAbs () _space (Special () _)) = error $ "exportToImport: " ++ prettyPrint x
exportToImport (EThingWith () (EWildcard () _) (UnQual () name) []) = IThingAll () name
exportToImport x@(EThingWith () (EWildcard () _) (Qual () _mname _name) []) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(EThingWith () (EWildcard () _) (Special () _) []) = error $ "exportToImport: " ++ prettyPrint x
exportToImport (EThingWith () (EWildcard () _) _ (_ : _)) = error "PatternSynonyms support not implemented"
exportToImport (EThingWith () (NoWildcard ()) (UnQual () name) cnames) = IThingWith () name cnames
exportToImport x@(EThingWith () (NoWildcard ()) (Qual () _mname _name) _cnames) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(EThingWith () (NoWildcard ()) (Special () _) _cnames) = error $ "exportToImport: " ++ prettyPrint x
exportToImport x@(EModuleContents () _) = error $ "exportToImport: " ++ prettyPrint x
-- exportToImport x = error $ "exportToImport: " ++ prettyPrint x

-- | Build an ImportDecl that imports the symbols of d from m.
importSpecFromDecl :: forall l. (SrcInfo l, Eq l, Data l, Show l) => ModuleInfo l -> ModuleName () -> Decl l -> Maybe (ImportDecl ())
importSpecFromDecl thisMod newModName d =
    case map exportToImport (maybeToList (topDeclExportSpec (dropAnn thisMod) (dropAnn d))) of
      [] -> Nothing
      imports -> Just (ImportDecl { importAnn = ()
                                  , importModule = dropAnn newModName
                                  , importQualified = False
                                  , importSrc = False
                                  , importSafe = False
                                  , importPkg = Nothing
                                  , importAs = Nothing
                                  , importSpecs = Just (ImportSpecList () False imports) })

-- | Given in import spec and the name of the module it was imported
-- from, return the name of the new module where it will now be
-- imported from.
newModuleOfImportSpec :: (SrcInfo l, Data l, Show l) => Rd l -> MoveSpec -> ModuleName () -> ImportSpec l -> Maybe (ModuleName ())
newModuleOfImportSpec (Rd mods _env _gr) mv oldModname spec =
    case findModuleByName mods oldModname of
      Just info -> case findDeclOfImportSpec info spec of
                     Just d -> moduleName (applyMoveSpec mv info d)
                     -- Everything else we can leave alone - even if we can't
                     -- find a declaration, they might be re-exported.
                     Nothing {- | isReexport info spec -} -> Just oldModname
                     -- Nothing -> trace ("Unable to find decl of " ++ prettyPrint' spec ++ " in " ++ show oldModname) Nothing
      -- If we don't know about the module leave the import spec alone
      Nothing -> Just oldModname

findModuleByName :: forall l. (Show l) => [ModuleInfo l] -> ModuleName () -> Maybe (ModuleInfo l)
findModuleByName mods oldModname =
    case filter (testModuleName oldModname) mods of
      [m] -> Just m
      [] -> Nothing
      _ms -> error $ "Multiple " ++ show oldModname
    where
      testModuleName :: ModuleName () -> ModuleInfo l -> Bool
      testModuleName modName (ModuleInfo {_module = Module _ (Just (ModuleHead _ name _ _)) _ _ _}) =
          dropAnn name == modName
      testModuleName modName (ModuleInfo {_module = Module _ Nothing _ _ _}) =
          modName == ModuleName () "Main"
      testModuleName _ x = error $ "findModuleByName - unexpected module: " ++ show (_module x)

-- | Find the declaration in a module that causes all the symbols in
-- the ImportSpec to come into existance.
findDeclOfImportSpec :: forall l. (SrcInfo l, Data l, Show l) => ModuleInfo l -> ImportSpec l -> Maybe (Decl l)
findDeclOfImportSpec info spec = findDeclOfSymbols info (Set.fromList (map dropAnn (gFind spec :: [Name l])))
    where
      findDeclOfSymbols :: ModuleInfo l -> Set (Name ()) -> Maybe (Decl l)
      findDeclOfSymbols _ syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = m}) syms =
          case filter (isSubsetOf syms . symset) (filter notSig (getModuleDecls m)) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map (srcLoc . ann) ds)
      symset = Set.fromList . map symbolName . getTopDeclSymbols' info

notSig :: Decl t -> Bool
notSig (TypeSig {}) = False
notSig _ = True

#if 0
-- Are each of the symbols of this import spec re-exported by
-- some export spec info?
isReexport :: ModuleInfo -> ImportSpec SrcSpanInfo -> Bool
isReexport info@(ModuleInfo {_module = Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ especs)))) _ _ _}) ispec =
    let syms = Set.fromList (gFind ispec :: [name SrcSpanInfo]) in
    all (isReexported especs) syms
    -- not (null (filter (isReexported syms) especs))
    -- (not . null . filter (isReexport' syms . foldDeclared Set.insert mempty)) specs

-- Is symbol re-exported?
isReexported :: [ExportSpec SrcSpanInfo] -> Name -> Bool
isReexported specs sym = any (reexports sym) specs

-- Does this export spec re-export this symbol?  (FIXME: Need to
-- change sym type to handle EThingAll.)
reexports :: Name -> ExportSpec SrcSpanInfo -> Bool
reexports sym e@(EThingAll _ qname) = trace ("EThingAll") $ Set.member sym (foldDeclared Set.insert mempty e)
reexports sym (EModuleContents _ _mname) = False
reexports sym e = Set.member sym (Set.fromList (gFind e :: [Name SrcSpanInfo]))
#endif

-- | Look through a module's imports, using findDeclOfImportSpec and
-- moveSpec to determine which refer to symbols that are moving from
-- one module to another.  There are three cases for an import that
-- moves.  It might move from another module to this module, in which
-- case it can be removed.  It might move between two modules other
-- than this one, in which case the a new import with the new module
-- name is added.  The final case is invalid - a module that imported
-- itself.
updateDecls :: (Data l, SrcInfo l, EndLoc l, Show l, EZPrint l) => Rd l -> MoveSpec -> ModuleInfo l -> ScanM ()
updateDecls (Rd mods _env _gr) mv thisMod@(ModuleInfo {_module = m, _moduleKey = thisKey}) = do
  -- keep (endOfImports m)
  -- Declarations that were already here and are to remain
  mapM_ (uncurry doDecl) (zip decls (tail (map (srcLoc . ann) decls ++ [endOfModule thisMod])))
  keepAll
  tell $ newDecls mv mods thisKey
    where
      decls = getModuleDecls m
      doDecl d next =
          case applyMoveSpec mv thisMod d of
            someKey | someKey /= thisKey -> do
              trace ("decl departing: " ++ ezPrint (thisMod, d) ++ " from " ++ ezPrint thisKey ++ " to " ++ ezPrint someKey) (pure ())
              skip (endLoc (ann d))
              withTrailingWhitespace skip next
            _ -> do
              keep (endLoc (ann d))
              withTrailingWhitespace keep next

-- | Declarations that are moving here from other modules.
newDecls :: forall l. (SrcInfo l, EndLoc l, Data l) => MoveSpec -> [ModuleInfo l] -> ModuleKey -> String
newDecls mv mods thisKey =
    concatMap doModule mods
    where
      -- Scan the declarations of all the modules except this one
      doModule someMod@(ModuleInfo {_module = (Module _l _mh _ps _is decls),
                                    _moduleKey = someKey})
          | someKey /= thisKey =
              scanModule (do skip (endOfImports (_module someMod))
                             mapM_ (uncurry (doDecl someMod)) (zip decls (tail (map (srcLoc . ann) decls ++ [endOfModule someMod]))))
                         someMod
      doModule _ = ""

      -- If a declaration is to be moved to this module, extract its
      -- text and add it to the result.
      doDecl :: ModuleInfo l -> Decl l -> SrcLoc -> ScanM ()
      doDecl someMod d next =
          case applyMoveSpec mv someMod d == thisKey of
            False -> do
              skip (endLoc (ann d))
              withTrailingWhitespace skip next
            True -> do
              keep (endLoc (ann d))
              withTrailingWhitespace keep next

{-
-- | Get the text of a declaration including the preceding whitespace
declText :: ModuleInfo SrcSpanInfo -> Decl SrcSpanInfo -> String
declText (ModuleInfo {_module = m@(Module _ mh ps is ds), _moduleText = mtext}) d =
    -- Find the end of the last object preceding d - could be the
    -- preceding declaration, the last import, the last pragma, or the
    -- module header.  If none of that exists use the module start
    -- location.
    let p = case ds of
              (d1 : _) | d == d1 -> endLoc (last (maybe [] (\x -> [ann x]) mh ++ map ann ps ++ map ann is))
              _ -> case dropWhile2 (\_  md2 -> Just d /= md2) ds of
                     (d1 : _) -> endLoc (ann d1)
                     [] -> srcLoc (ann m) in
    textOfSpan (mkSrcSpan p (endLoc (ann d))) mtext
declText x _ = error $ "declText - unexpected module: " ++ show (_module x)
-}
