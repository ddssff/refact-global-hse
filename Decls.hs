{-# LANGUAGE CPP, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
module Decls (moveDeclsAndClean, moveDecls) where

import Control.Exception (SomeException)
import Control.Lens ((.=), makeLenses, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS, when)
import Data.List (nub)
import Data.Map as Map (delete, findWithDefault, fromListWith, Map, toList)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set as Set (insert, isSubsetOf, member, Set, singleton, toList, union)
import Imports (cleanImports)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Simplify (sCName, sImportDecl, sImportSpec, sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintStyleMode)
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S
import SrcLoc (endLoc, spanText, srcLoc, textSpan)
import Symbols (FoldDeclared(foldDeclared))
import System.FilePath.Extra (replaceFile)
import Text.PrettyPrint (mode, Mode(OneLineMode), style)
import Types (ModuleInfo(ModuleInfo, _module, _moduleKey, _moduleText), ModuleKey(_moduleName, _modulePath), loadModule)
import Utils (dropWhile2)

prettyPrint' :: A.Pretty a => a -> String
prettyPrint' = prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode

data S =
    S
    { _hiding :: Bool
    , _subterms :: [String]
    , _point :: SrcLoc }

$(makeLenses ''S)

moveDeclsAndClean :: MoveSpec -> FilePath -> [ModuleInfo] -> IO ()
moveDeclsAndClean moveSpec scratch modules = do
  -- Move the declarations and rewrite the updated modules
  paths <- mapM (\(m, s) -> do
                   let p = _modulePath (_moduleKey m)
                   case _moduleText m == s of
                     True -> return Nothing
                     False -> replaceFile p s >> return (Just p))
                 (zip modules (moveDecls moveSpec modules))
  -- Re-read the updated modules and clean their imports
  -- (Later we will need to find the newly created modules here)
  modules' <- mapM (\p -> either (loadError p) id <$> loadModule p) (catMaybes paths) :: IO [ModuleInfo]
  cleanImports scratch modules'
    where
      loadError :: FilePath -> SomeException -> ModuleInfo
      loadError p e = error ("Unable to load updated module " ++ show p ++ ": " ++ show e)

-- | Specifies where to move each declaration of each module.
type MoveSpec = ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: MoveSpec -> [ModuleInfo] -> [String]
moveDecls moveSpec modules = map (\info -> moveDeclsOfModule moveSpec modules info) modules

-- Update one module
moveDeclsOfModule :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
moveDeclsOfModule moveSpec modules info@(ModuleInfo {_module = A.Module l h _ i ds}) =
    snd $ evalRWS (do tell' (srcLoc l)
                      newHeader moveSpec modules info h
                      newImports moveSpec modules info i
                      newDecls moveSpec modules info ds
                      t <- view id
                      tell' (endLoc (textSpan (srcFilename (endLoc l)) t))
                  )
                  (_moduleText info)
                  (S False [] ((srcLoc l) {srcLine = 1, srcColumn = 1}))

tell' :: SrcLoc -> RWS String String S ()
tell' l = do
  t <- view id
  p <- use point
  tell (spanText (p, l) t)
  point .= l

-- | Write the new export list.  Exports of symbols that have moved
-- out are removed.  Exports of symbols that have moved in are added
-- *if* the symbol is imported anywhere else.
newHeader :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> Maybe (A.ModuleHead SrcSpanInfo) -> RWS String String S ()
newHeader moveSpec modules m (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList l specs)))) = do
  tell' (srcLoc l) -- write everything to beginning of first export
  mapM_ (doExport moveSpec m) specs
  (tell . concatMap ((sep ++) . prettyPrint) . nub) (newExports moveSpec modules m)
  -- mapM_ newExports (filter (\x -> _moduleKey x /= _moduleKey m) modules)
    where
      -- This should be inferred from the module text
      sep = "\n    , "
      -- exportSpecText :: A.Decl SrcSpanInfo -> String
      -- exportSpecText d = (concatMap ((", " ++) . prettyPrint) . nub . foldDeclared (:) []) d
newHeader _ _ _ _ = pure ()

newExports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> [S.Name]
newExports moveSpec modules m =
    concatMap newExportsFromModule (filter (\x -> _moduleKey x /= _moduleKey m) modules)
    where
      k = _moduleKey m
      -- Scan a module other than m for declarations moving to m.  If
      -- found, transfer the export from there to here.
      newExportsFromModule :: ModuleInfo -> [S.Name]
      newExportsFromModule (ModuleInfo {_moduleKey = k', _module = A.Module _l _h _ _i ds}) =
          concatMap (\d -> if (moveSpec k' d == k) then foldDeclared (:) [] d else []) ds

-- | Find the declaration of the export spec in the current module.
-- If it is not there, it is a re-export which we just keep.
doExport :: MoveSpec -> ModuleInfo -> A.ExportSpec SrcSpanInfo -> RWS String String S ()
doExport moveSpec info@(ModuleInfo {_moduleKey = k}) spec =
    case findNewKeyOfExportSpec moveSpec info spec of
      Nothing -> (tell' . endLoc . A.ann) spec
      Just k' | k' == k -> (tell' . endLoc . A.ann) spec
      _ -> point .= (endLoc . A.ann) spec

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
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ decls}) syms | null syms = Nothing
      findDeclOfSymbols (ModuleInfo {_module = A.Module _ _ _ _ decls}) syms =
          case filter (isSubsetOf syms . foldDeclared Set.insert mempty) (filter notSig decls) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map srcLoc ds)

skip :: SrcLoc -> RWS String String S ()
skip loc = point .= loc

-- Find the declaration of the symbols of an import spec.  If that
-- declaration moved, update the module name.
newImports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> [A.ImportDecl SrcSpanInfo] -> RWS String String S ()
newImports moveSpec modules (ModuleInfo {_moduleKey = k}) imports = do
  mapM_ doImportDecl imports
  mapM_ doNewImports (filter (\m -> _moduleKey m /= k) modules)
    where
      -- If a declaration is moving from another module to this one, all of
      -- that module's imports must be duplicated here.  Also, all of that modules
      -- exports must be turned into imports here (with updated module names)
      doNewImports :: ModuleInfo -> RWS String String S ()
      doNewImports m@(ModuleInfo {_moduleKey = k', _module = A.Module _ mh _ is@(_ : _) ds}) =
          when (any (\d -> moveSpec k' d == k) ds) $ do
            tell ("\n" ++ spanText (mkSrcSpan (srcLoc (head is)) (endLoc (last is))) (_moduleText m))
            case maybe [] (\(A.ModuleHead _ _ _ mesl) -> maybe [] (\(A.ExportSpecList _ especs) -> especs) mesl) mh of
              [] -> pure ()
              especs ->
                  -- Add imports of the exports whose declarations stayed put
                  tell ("\n" ++
                        prettyPrint (S.ImportDecl
                                          { S.importLoc = srcLoc (_module m)
                                          , S.importModule = _moduleName k'
                                          , S.importQualified = False
                                          , S.importSrc = False
                                          , S.importSafe = False
                                          , S.importPkg = Nothing
                                          , S.importAs = Nothing
                                          , S.importSpecs = Just (False, mapMaybe toImportSpec (filter (eSpecPred moveSpec m) especs)) }))

      eSpecPred :: MoveSpec -> ModuleInfo -> A.ExportSpec SrcSpanInfo -> Bool
      eSpecPred moveSpec m espec = (findNewKeyOfExportSpec moveSpec m espec == Just (_moduleKey m))

      -- Build an import spec corresponding to an export spec.
      toImportSpec :: A.ExportSpec SrcSpanInfo -> Maybe S.ImportSpec
      toImportSpec (A.EVar _ (A.Qual _ mname name)) = Just $ S.IVar (sName name)
      toImportSpec (A.EVar _ (A.UnQual _ name)) = Just $ S.IVar (sName name)
      toImportSpec (A.EAbs _ space (A.UnQual _ name)) = Nothing
      toImportSpec (A.EAbs _ space (A.Qual _ mname name)) = Nothing
      toImportSpec (A.EThingAll _ (A.UnQual _ name)) = Just $ S.IThingAll (sName name)
      toImportSpec (A.EThingAll _ (A.Qual _ mname name)) = Nothing
      toImportSpec (A.EThingWith _ (A.UnQual _ name) cnames) = Just $ S.IThingWith (sName name) (map sCName cnames)
      toImportSpec (A.EThingWith _ (A.Qual _ mname name) cnames) = Nothing
      toImportSpec _ = Nothing

      doImportDecl :: A.ImportDecl SrcSpanInfo -> RWS String String S ()
      doImportDecl x@(A.ImportDecl {importSpecs = Nothing}) = (tell' . endLoc . A.ann) x
      doImportDecl x@(A.ImportDecl {importModule = name, importSpecs = Just (A.ImportSpecList _ hiding specs)}) =
          do let oldModname = sModuleName name
             -- Build a map from module key to set of specs to be
             -- imported from it.
             let mp :: Map S.ModuleName (Set (A.ImportSpec SrcSpanInfo))
                 mp = Map.fromListWith Set.union
                        (mapMaybe g (zip (map (newModuleOfImportSpec moveSpec modules oldModname) specs)
                                         (map singleton specs)))
                 g :: (Maybe k, a) -> Maybe (k, a)
                 g (Nothing, _) = Nothing
                 g (Just k, a) = Just (k, a)
             -- Some specs will need to be imported
             -- via new ImportDecls to appear after this one.
             (tell' . srcLoc . A.ann) x
             -- Render the specs of the import decl that haven't been moved
             mapM_ (\spec -> if Set.member spec (Map.findWithDefault mempty oldModname mp)
                             then (tell' . endLoc . A.ann) spec
                             else (skip . endLoc . A.ann) spec) specs
             -- Output the portion of the ImportDecl following the last ImportSpec.
             (tell' . endLoc . A.ann) x
             -- Output a new import decl for each symbol that has moved
             mapM_ (\(name, specs) -> do
                      tell "\n"
                      (tell . prettyPrint') (sImportDecl x) {S.importModule = name, S.importSpecs = Just (hiding, map sImportSpec (Set.toList specs))})
                   (Map.toList (Map.delete oldModname mp))

-- | Given in import spec and the name of the module it was imported
-- from, return the name of the new module where it will now be
-- imported from.
newModuleOfImportSpec :: MoveSpec -> [ModuleInfo] -> S.ModuleName -> A.ImportSpec SrcSpanInfo -> Maybe S.ModuleName
newModuleOfImportSpec moveSpec modules oldModname spec =
    case findModuleByName modules oldModname of
      Just info -> case findDeclOfImportSpec info spec of
                     Just d -> Just (_moduleName (moveSpec (_moduleKey info) d))
                     -- Everything else we can leave alone - even if we can't
                     -- find a declaration, they might be re-exported.
                     Nothing {- | isReexport info spec -} -> Just oldModname
                     -- Nothing -> trace ("Unable to find decl of " ++ prettyPrint' spec ++ " in " ++ show oldModname) Nothing
      -- If we don't know about the module leave the import spec alone
      Nothing -> Just oldModname
    where
      findModuleByName :: [ModuleInfo] -> S.ModuleName -> Maybe ModuleInfo
      findModuleByName modules oldModname =
          case filter (testModuleName oldModname) modules of
            [m] -> Just m
            [] -> Nothing
            ms -> error $ "Multiple " ++ show oldModname
          where
            testModuleName :: S.ModuleName -> ModuleInfo -> Bool
            testModuleName modName info@(ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ name _ _)) _ _ _}) =
                sModuleName name == modName
            testModuleName modName info@(ModuleInfo {_module = A.Module _ Nothing _ _ _}) =
                modName == S.ModuleName "Main"

-- | Find the declaration in a module that causes all the symbols in
-- the ImportSpec to come into existance.
findDeclOfImportSpec :: ModuleInfo -> A.ImportSpec SrcSpanInfo -> Maybe (A.Decl SrcSpanInfo)
findDeclOfImportSpec info spec = findDeclOfSymbols info (foldDeclared Set.insert mempty spec)
    where
      findDeclOfSymbols :: ModuleInfo -> Set S.Name -> Maybe (A.Decl SrcSpanInfo)
      findDeclOfSymbols info@(ModuleInfo {_module = A.Module _ _ _ _ decls}) syms | null syms = Nothing
      findDeclOfSymbols info@(ModuleInfo {_module = A.Module _ _ _ _ decls}) syms =
          case filter (isSubsetOf syms . foldDeclared Set.insert mempty) (filter notSig decls) of
            [d] -> Just d
            [] -> Nothing
            ds -> error $ "Multiple declarations of " ++ show syms ++ " found: " ++ show (map srcLoc ds)

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
newDecls :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> [A.Decl SrcSpanInfo] -> RWS String String S ()
newDecls moveSpec modules info decls = do
  oldDecls
  newDecls
    where
      -- Declarations that were already here and are to remain
      oldDecls :: RWS String String S ()
      oldDecls = mapM_ (\d -> case moveSpec (_moduleKey info) d of
                                k | k == _moduleKey info -> tell' (endLoc d)
                                _ -> point .= endLoc d) decls
      -- Declarations that are moving here from other modules.
      -- We have to scan all the modules we know about for this.
      newDecls :: RWS String String S ()
      newDecls = mapM_ (\m@(ModuleInfo {_module = A.Module mspan _ _ _ decls}) ->
                            mapM_ (\d -> case moveSpec (_moduleKey m) d of
                                           k | k == _moduleKey info -> do
                                             tell (declText m d)
                                           k -> pure ()) decls)
                       (filter (\m -> _moduleKey m /= _moduleKey info) modules)

-- | Get the text of a declaration including the preceding whitespace
declText :: ModuleInfo -> A.Decl SrcSpanInfo -> String
declText (ModuleInfo {_module = m@(A.Module _ mh ps is ds), _moduleText = mtext}) d =
    -- Find the end of the last object preceding d - could be the
    -- preceding declaration, the last import, the last pragma, or the
    -- module header.  If none of that exists use the module start
    -- location.
    let p = case ds of
              (d1 : _) | d == d1 -> endLoc (last (maybe [] (\x -> [A.ann x]) mh ++ map A.ann ps ++ map A.ann is))
              ds -> case dropWhile2 (\_  md2 -> Just d /= md2) ds of
                      (d1 : d2 : _) -> endLoc d1
                      [] -> srcLoc (A.ann m) in
    spanText (mkSrcSpan p (endLoc d)) mtext
