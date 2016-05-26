{-# LANGUAGE CPP, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
module Decls (makeMoveSpec, moveDeclsAndClean, moveDecls, ) where

import Control.Exception (SomeException)
import Control.Lens ((.=), makeLenses, use, view)
import Control.Monad (void)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Data.Foldable (find)
import Data.List (foldl', intercalate, nub)
import Data.Map as Map (insertWith, Map, mapWithKey, singleton, toList)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set)
import Data.Tuple (swap)
import Debug.Trace (trace)
import IO (replaceFile)
import Imports (cleanImports)
import qualified Language.Haskell.Exts.Annotated as A (Annotated(ann), Decl(TypeSig), ExportSpec(EAbs, EThingAll, EThingWith, EVar), ExportSpecList(ExportSpecList), ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec, ImportSpecList(ImportSpecList), Module(Module), ModuleHead(ModuleHead), Pretty, QName(Qual, UnQual))
import Language.Haskell.Exts.Annotated.Simplify (sCName, sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintStyleMode)
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(..), ImportSpec(IThingAll, IThingWith, IVar), ModuleName(..), Name(..))
import MoveSpec (MoveSpec)
import SrcLoc (endLoc, spanText, srcLoc, textSpan)
import Symbols (FoldDeclared(foldDeclared))
import Text.PrettyPrint (mode, Mode(OneLineMode), style)
import Types (ModuleKey(ModuleKey, _moduleName), fullPathOfModuleKey, loadModule, ModuleInfo(..))
import Utils (dropWhile2)


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

-- A simple MoveSpec builder.
makeMoveSpec :: String -> String -> String -> MoveSpec
makeMoveSpec fname mname mname' =
    \mkey decl ->
        let syms = foldDeclared Set.insert mempty decl in
        if _moduleName mkey == Just (S.ModuleName mname) && (Set.member (S.Ident fname) syms || Set.member (S.Symbol fname) syms)
        then {-t1 mkey decl-} (mkey {_moduleName = Just (S.ModuleName mname')})
        else mkey
    -- where
      -- t1 mkey decl x = trace ("moveSpec " ++ show mkey ++ " " ++ show (foldDeclared (:) [] decl) ++ " -> " ++ show x) x

prettyPrint' :: A.Pretty a => a -> String
prettyPrint' = prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode

data St = St { _point :: SrcLoc, _newmods :: Map ModuleKey [A.Decl SrcSpanInfo] }

$(makeLenses ''St)

moveDeclsAndClean :: MoveSpec -> FilePath -> [ModuleInfo] -> IO ()
moveDeclsAndClean moveSpec scratch modules = do
  -- Move the declarations and rewrite the updated modules
  oldPaths <-
      mapM (\(m, s) -> do
              let p = _modulePath m
              case _moduleText m == s of
                True -> return Nothing
                False -> replaceFile p s >> return (Just p))
           (zip modules (moveDecls moveSpec modules))
  newPaths <- mapM (\(k, s) -> do
                      let path = fullPathOfModuleKey k
                      void $ replaceFile (trace ("New file: " ++ show path) path) s
                      pure path)
                   (Map.toList (newModuleMap moveSpec modules))
  -- Re-read the updated modules and clean their imports
  -- (Later we will need to find the newly created modules here)
  modules' <- mapM (\p -> either (loadError p) id <$> loadModule p) (catMaybes oldPaths ++ newPaths) :: IO [ModuleInfo]
  cleanImports scratch modules'
    where
      loadError :: FilePath -> SomeException -> ModuleInfo
      loadError p e = error ("Unable to load updated module " ++ show p ++ ": " ++ show e)

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: MoveSpec -> [ModuleInfo] -> [String]
moveDecls moveSpec modules = map (\info -> moveDeclsOfModule moveSpec modules info) modules

-- | Update one module and return its text
moveDeclsOfModule :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
moveDeclsOfModule moveSpec modules info@(ModuleInfo {_module = A.Module l _ _ _ _}) =
    -- let modules' = newModules moveSpec modules ++ modules in
    snd $ evalRWS (do keep (srcLoc l)
                      updateHeader moveSpec modules info
                      updateImports moveSpec modules info
                      updateDecls moveSpec modules info
                      t <- view id
                      keep (endLoc (textSpan (srcFilename (endLoc l)) t))
                  )
                  (_moduleText info)
                  (St {_point = ((srcLoc l) {srcLine = 1, srcColumn = 1}), _newmods = mempty})
moveDeclsOfModule _ _ x = error $ "moveDeclsOfModule - unexpected module: " ++ show (_module x)

-- | Unsafe ModuleInfo lookup
findModuleByKey :: [ModuleInfo] -> ModuleKey -> Maybe ModuleInfo
findModuleByKey modules thisKey = find (\m -> _moduleKey m == thisKey) modules

findModuleByKeyUnsafe :: [ModuleInfo] -> ModuleKey -> ModuleInfo
findModuleByKeyUnsafe modules thisKey = maybe (error $ "Module not found: " ++ show thisKey) id $ findModuleByKey modules thisKey

exportSep :: String
exportSep = "\n    , "

-- | Build the new modules
newModuleMap :: MoveSpec -> [ModuleInfo] -> Map ModuleKey String
newModuleMap moveSpec modules =
    textMap
    where
      textMap :: Map ModuleKey String
      textMap = Map.mapWithKey (\k ds -> doModule k ds) declMap
          where
            doModule :: ModuleKey -> [(ModuleKey, A.Decl SrcSpanInfo)] -> String
            -- Build module thiskey from the list of (fromkey, decl) pairs
            doModule thisKey pairs =
                "module " ++ maybe "Main" prettyPrint (_moduleName thisKey) ++ "(" ++
                intercalate exportSep (newExports moveSpec modules thisKey) ++ "\n    ) where\n" ++
                concatMap (\(someKey, ds) -> importsForArrivingDecls moveSpec thisKey [] (findModuleByKeyUnsafe modules someKey)) pairs ++
                -- importsForDepartingDecls moveSpec modules thisKey ++  new module, no decls can depart
                newDecls moveSpec modules thisKey ++ "\n"
      declMap :: Map ModuleKey [(ModuleKey, A.Decl SrcSpanInfo)]
      declMap = foldl' doModule mempty modules
          where
            doModule :: Map ModuleKey [(ModuleKey, A.Decl SrcSpanInfo)] -> ModuleInfo -> Map ModuleKey [(ModuleKey, A.Decl SrcSpanInfo)]
            doModule mp (ModuleInfo {_module = A.Module _ _ _ _ ds, _moduleKey = k}) = foldl' (doDecl k) mp ds
            doModule mp _ = mp
            doDecl k mp d = let k' = moveSpec k d in
                            if Set.member k' oldKeys then mp else Map.insertWith (++) k' [(k, d)] mp
      oldKeys :: Set ModuleKey
      oldKeys = Set.fromList (map _moduleKey modules)

-- | What is the best place to put a newly created module?
defaultHsSourceDir :: [ModuleInfo] -> FilePath
defaultHsSourceDir modules =
    let countMap = foldl' (\mp path -> Map.insertWith (+) path (1 :: Int) mp)
                          (Map.singleton "." 0)
                          (map _modulePath modules) in
    snd (maximum (map swap (Map.toList countMap)))

keep :: SrcLoc -> RWS String String St ()
keep l = do
  t <- view id
  p <- use point
  tell (spanText (p, l) t)
  point .= l

-- | Write the new export list.  Exports of symbols that have moved
-- out are removed.  Exports of symbols that have moved in are added
-- *if* the symbol is imported anywhere else.
updateHeader :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> RWS String String St ()
updateHeader moveSpec modules m@(ModuleInfo {_moduleKey = k, _module = A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList l specs)))) _ _ _}) = do
  keep (srcLoc l) -- write everything to beginning of first export
  mapM_ doExport specs
  tell (concat (map (exportSep ++) (newExports moveSpec modules (_moduleKey m))))
  -- mapM_ newExports (filter (\x -> _moduleKey x /= _moduleKey m) modules)
    where
      -- exportSpecText :: A.Decl SrcSpanInfo -> String
      -- exportSpecText d = (concatMap ((", " ++) . prettyPrint) . nub . foldDeclared (:) []) d
      -- Find the declaration of the export spec in the current module.
      -- If it is not there, it is a re-export which we just keep.
      doExport :: A.ExportSpec SrcSpanInfo -> RWS String String St ()
      doExport spec =
          case findNewKeyOfExportSpec moveSpec m spec of
            Nothing -> (keep . endLoc . A.ann) spec
            Just k' | k' == k -> (keep . endLoc . A.ann) spec
            _ -> point .= (endLoc . A.ann) spec
updateHeader _ _ _ = pure ()

-- | Text of exports added due to arriving declarations
newExports :: MoveSpec -> [ModuleInfo] -> ModuleKey -> [String]
newExports moveSpec modules thisKey =
    map prettyPrint names
    where
      -- This should be inferred from the module text
      names = nub $ concatMap newExportsFromModule (filter (\x -> _moduleKey x /= thisKey) modules)
      -- Scan a module other than m for declarations moving to m.  If
      -- found, transfer the export from there to here.
      newExportsFromModule :: ModuleInfo -> [S.Name]
      newExportsFromModule (ModuleInfo {_moduleKey = k', _module = A.Module _l _h _ _i ds}) =
          concatMap (\d -> if (moveSpec k' d == thisKey) then foldDeclared (:) [] d else []) ds
      newExportsFromModule x = error $ "newExports - unexpected module: " ++ show (_module x)

findNewKeyOfExportSpec :: MoveSpec -> ModuleInfo -> A.ExportSpec SrcSpanInfo -> Maybe ModuleKey
findNewKeyOfExportSpec moveSpec info@(ModuleInfo {_moduleKey = k}) spec =
    fmap (moveSpec k) (findDeclOfExportSpec info spec)

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

skip :: SrcLoc -> RWS String String St ()
skip loc = point .= loc

-- Find the declaration of the symbols of an import spec.  If that
-- declaration moved, update the module name.
updateImports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> RWS String String St ()
updateImports moveSpec modules thisModule@(ModuleInfo {_moduleKey = thisKey@(ModuleKey {_moduleName = thisModuleName}),
                                                       _module = A.Module _ _ _ thisModuleImports _}) = do
  -- Update the existing imports
  mapM_ doImportDecl thisModuleImports
  mapM_ doNewImports (filter (\m -> _moduleKey m /= thisKey) modules)
  tell $ importsForDepartingDecls moveSpec modules (findModuleByKey modules thisKey)
    where
      -- Process one import declaration.  Each of its specs will
      -- either be kept, discarded, or moved to a new import with a
      -- new module name.
      doImportDecl :: A.ImportDecl SrcSpanInfo -> RWS String String St ()
      doImportDecl x@(A.ImportDecl {importSpecs = Nothing}) = (keep . endLoc . A.ann) x
      doImportDecl x@(A.ImportDecl {importModule = name, importSpecs = Just (A.ImportSpecList _ hiding specs)}) =
          do (keep . srcLoc . A.ann) x
             mapM_ (doImportSpec (sModuleName name) hiding) specs
             (keep . endLoc . A.ann) x
             mapM_ (importsForMovingDecls (sModuleName name) hiding) specs

      doImportSpec :: S.ModuleName -> Bool -> A.ImportSpec SrcSpanInfo -> RWS String String St ()
      doImportSpec name hiding spec =
          case newModuleOfImportSpec moveSpec modules name spec of
            Just name'
                | -- Retain import if module name is unchanged
                  name' == name -> (keep . endLoc . A.ann) spec
            _ -> (skip . endLoc . A.ann) spec

      importsForMovingDecls :: S.ModuleName -> Bool -> A.ImportSpec SrcSpanInfo -> RWS String String St ()
      importsForMovingDecls name hiding spec =
          case newModuleOfImportSpec moveSpec modules name spec of
            Just name'
                | -- has import module name changed?
                  name' /= name &&
                  -- don't generate self imports
                  Just name' /= thisModuleName ->
                    tell ("\nimport " ++ prettyPrint' name' ++ " (" ++ prettyPrint' spec ++ ")")
            _ -> pure ()

      -- Add new imports due to declarations moving from someKey to
      -- thisKey.  All of the imports in someKey must be duplicated in
      -- thisKey (except for imports of thisKey).  Also, all of the
      -- exports in someKey must be turned into imports to thisKey
      -- (with updated module names.)  Finally, if a declaration
      -- moves from thisKey to someKey, we need to add an import of it
      -- here (as long as someKey doesn't import thisKey)
      doNewImports :: ModuleInfo -> RWS String String St ()
      doNewImports someModule = do
        tell $ importsForArrivingDecls moveSpec thisKey thisModuleImports someModule
updateImports _ _ x = error $ "updateImports - unexpected module: " ++ show (_module x)

-- | If a decl is move from thisModule to someModule we may need to
-- import its symbols to satisfy remaining declarations that use it.
-- However, if someModule already imports thisModule that will cause
-- an import cycle.  Unfortunately, even if someModule *does not* import
-- thisModule a cycle can appear, because of the code that converts the
-- exports of someModule into imports in thisModule.
importsForDepartingDecls :: MoveSpec -> [ModuleInfo] -> Maybe ModuleInfo -> String
importsForDepartingDecls moveSpec modules (Just thisModule@(ModuleInfo {_moduleKey = thisKey@(ModuleKey {_moduleName = Just thisModuleName}), _module = A.Module _ _ _ _ ds})) =
    concatMap (\d -> case moveSpec thisKey d of
                       someKey@(ModuleKey {_moduleName = Just someModuleName})
                           | someKey /= thisKey ->
                               case findModuleByKey modules someKey of
                                 Just someModule@(ModuleInfo {_module = A.Module _ _ _ someModuleImports _}) ->
                                     case moveType someModuleImports thisModuleName of
                                       Down ->  "\n" ++ prettyPrint' (importSpecFromDecl someModuleName d)
                                       Up -> ""
                                 Nothing -> ""
                       _ -> "") ds
importsForDepartingDecls _ _ _ = ""

moveType :: [A.ImportDecl SrcSpanInfo] -> S.ModuleName -> MoveType
moveType arrivalModuleImports departureModuleName =
    case arrivalModuleImports `importsSymbolsFrom` departureModuleName of
      -- If the arrival module imports from the departure module this
      -- is an Up move - we are moving the declaration towards
      -- somewhere it is used, and we must not leave behind or add any
      -- references to it.
      True -> Up
      -- If the arrival module does not import the departure module
      -- this *may* be a Down move.  However, it may still be that
      -- there are remaining references to the declaration in the
      -- departure module.
      False -> Down

importsForArrivingDecls :: MoveSpec -> ModuleKey -> [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> String
importsForArrivingDecls moveSpec thisKey thisModuleImports someModule@(ModuleInfo {_moduleKey = someKey, _module = A.Module _ _ _ is ds}) =
    if any (\d -> moveSpec someKey d == thisKey) ds
    then concatMap
             (\i -> "\n" ++ prettyPrint' i)
             (filter (\i -> _moduleName thisKey /= Just (sModuleName (A.importModule i))) is) ++
         concatMap
             (\i -> "\n" ++ prettyPrint' i)
             (maybeToList (importDeclFromExportSpecs moveSpec thisModuleImports someModule))
    else ""
importsForArrivingDecls _ _ _ _ = ""

-- | If a declaration moves from someModule to thisModule, and nothing
-- in thisModule is imported by someModule, add imports to thisModule
-- of all the symbols exported by someModule.
importDeclFromExportSpecs :: MoveSpec -> [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> Maybe S.ImportDecl
importDeclFromExportSpecs moveSpec
                          thisModuleImports
                          someInfo@(ModuleInfo
                                    {_moduleKey = someKey@(ModuleKey {_moduleName = Just someModuleName}),
                                     _module = someModule@(A.Module _ (Just (A.ModuleHead
                                                                              _ _ _ (Just (A.ExportSpecList _ especs@(_ : _))))) _ _ _)}) =
    -- Do we need to import the remaining exports from the departure
    -- module into the arrival module?  Only if we are moving the
    -- declaration 'Up', which implies that it may use symbols from
    -- the departure module.
    case moveType thisModuleImports someModuleName of
      Up -> Just (S.ImportDecl { S.importLoc = srcLoc someModule
                               , S.importModule = maybe (S.ModuleName "Main") id (_moduleName someKey)
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
          mapMaybe toImportSpec (filter (\e -> findNewKeyOfExportSpec moveSpec someInfo e == Just (_moduleKey someInfo)) especs)
          where
            -- These cases probably need work.
            toImportSpec :: A.ExportSpec SrcSpanInfo -> Maybe S.ImportSpec
            toImportSpec (A.EVar _ (A.Qual _ _mname name)) = Just $ S.IVar (sName name)
            toImportSpec (A.EVar _ (A.UnQual _ name)) = Just $ S.IVar (sName name)
            toImportSpec (A.EAbs _ _space (A.UnQual _ _name)) = Nothing
            toImportSpec (A.EAbs _ _space (A.Qual _ _mname _name)) = Nothing
            toImportSpec (A.EThingAll _ (A.UnQual _ name)) = Just $ S.IThingAll (sName name)
            toImportSpec (A.EThingAll _ (A.Qual _ _mname _name)) = Nothing
            toImportSpec (A.EThingWith _ (A.UnQual _ name) cnames) = Just $ S.IThingWith (sName name) (map sCName cnames)
            toImportSpec (A.EThingWith _ (A.Qual _ _mname _name) _cnames) = Nothing
            toImportSpec _ = Nothing
importDeclFromExportSpecs _ _ _ = Nothing

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
                 , S.importSpecs = Just (False, map S.IVar (foldDeclared (:) [] d)) }

-- | Given in import spec and the name of the module it was imported
-- from, return the name of the new module where it will now be
-- imported from.
newModuleOfImportSpec :: MoveSpec -> [ModuleInfo] -> S.ModuleName -> A.ImportSpec SrcSpanInfo -> Maybe S.ModuleName
newModuleOfImportSpec moveSpec modules oldModname spec =
    case findModuleByName modules oldModname of
      Just info -> case findDeclOfImportSpec info spec of
                     Just d -> _moduleName (moveSpec (_moduleKey info) d)
                     -- Everything else we can leave alone - even if we can't
                     -- find a declaration, they might be re-exported.
                     Nothing {- | isReexport info spec -} -> Just oldModname
                     -- Nothing -> trace ("Unable to find decl of " ++ prettyPrint' spec ++ " in " ++ show oldModname) Nothing
      -- If we don't know about the module leave the import spec alone
      Nothing -> Just oldModname

findModuleByName :: [ModuleInfo] -> S.ModuleName -> Maybe ModuleInfo
findModuleByName modules oldModname =
    case filter (testModuleName oldModname) modules of
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
updateDecls :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> RWS String String St ()
updateDecls moveSpec modules info@(ModuleInfo {_module = A.Module _ _ _ _ decls}) = do
  -- Declarations that were already here and are to remain
  mapM_ (\d -> case moveSpec (_moduleKey info) d of
                 k | k /= _moduleKey info -> do
                   -- trace ("Moving " ++ show (foldDeclared (:) [] d) ++ " to " ++ show (k) ++ " from " ++ show ((_moduleKey info))) (pure ())
                   point .= endLoc d
                 _ -> keep (endLoc d)) decls
  tell $ newDecls moveSpec modules (_moduleKey info)
updateDecls _ _ x = error $ "updateDecls - unexpected module: " ++ show (_module x)

-- | Declarations that are moving here from other modules.
newDecls :: MoveSpec -> [ModuleInfo] -> ModuleKey -> String
newDecls moveSpec modules thisKey =
    concatMap (\m@(ModuleInfo {_module = A.Module _mspan _ _ _ decls'}) ->
                   concatMap (\d -> case moveSpec (_moduleKey m) d of
                                      k | k /= thisKey -> ""
                                      _ -> declText m d) decls')
              (filter (\m -> _moduleKey m /= thisKey) modules)

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
    spanText (mkSrcSpan p (endLoc d)) mtext
declText x _ = error $ "declText - unexpected module: " ++ show (_module x)
