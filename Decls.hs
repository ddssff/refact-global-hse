{-# LANGUAGE CPP, RankNTypes, RecordWildCards, TemplateHaskell, TupleSections #-}
module Decls (moveDeclsAndClean, moveDecls) where

import Debug.Trace
import Control.Exception (SomeException)
import Control.Lens hiding ((|>))
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Generics
import Data.List (find, foldl', groupBy, intercalate, nub, sortBy)
import Data.Map as Map (delete, fromListWith, findWithDefault, insertWith, Map, toList)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>), (<|))
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set, singleton, toList, union, unions)
import qualified Data.Set as Set (map)
import Fold (foldDecls, foldExports, foldHeader, foldImports, echo2, echo, ignore, ignore2)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Simplify as S
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.Annotated.ExactPrint (exactPrint)
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrint, prettyPrintWithMode, prettyPrintStyleMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..), SrcLoc(..), mkSrcSpan)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importLoc, importModule, importSpecs), ModuleName(..), Name(..))
import SrcLoc (srcLoc, endLoc, spanText, splitText, textSpan, gFind)
import Symbols (FoldDeclared(foldDeclared), symbolsDeclaredBy)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.FilePath.Extra (replaceFile)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint (mode, Mode(OneLineMode), style)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Types (DerivDeclTypes(derivDeclTypes), hseExtensions, hsFlags, loadModule,
              ModuleInfo(..), ModuleKey(_modulePath, _moduleTop, _moduleName))
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
moveDeclsAndClean f scratch modules =
    mapM_ (\(m, s) -> if _moduleText m /= s then replaceFile (_modulePath (_moduleKey m)) s else pure ())
          (moveDecls f modules)

-- | Specifies where to move each declaration of each module.
type MoveSpec = ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: MoveSpec -> [ModuleInfo] -> [(ModuleInfo, String)]
moveDecls f modules = map (\info -> (info, moveDeclsOfModule f modules info)) modules

-- Update one module
moveDeclsOfModule :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
moveDeclsOfModule f modules info@(ModuleInfo {_module = m@(A.Module l h _ i ds)}) =
    snd $ evalRWS (do tell' (srcLoc l)
                      newHeader f modules info h
                      newImports f modules info i
                      newDecls f modules info ds
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
newHeader f modules m@(ModuleInfo {_moduleKey = k, _module = A.Module _ _ _ _ ds})
              (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList l specs)))) = do
  tell' (srcLoc l) -- write everything to beginning of first export
  mapM_ (doExport f m) specs
  let syms = concatMap newExports (filter (\x -> _moduleKey x /= _moduleKey m) modules)
  (tell . concatMap ((sep ++) . prettyPrint) . nub) syms
  -- mapM_ newExports (filter (\x -> _moduleKey x /= _moduleKey m) modules)
    where
      -- This should be inferred from the module text
      sep = "\n    , "
      -- Scan a module other than m for declarations moving to m.  If
      -- found, transfer the export from there to here.
      newExports :: ModuleInfo -> [S.Name]
      newExports m'@(ModuleInfo {_moduleKey = k', _module = A.Module l h _ i ds}) =
          concatMap (\d -> if (f k' d == k) then foldDeclared (:) [] d else []) ds
      -- exportSpecText :: A.Decl SrcSpanInfo -> String
      -- exportSpecText d = (concatMap ((", " ++) . prettyPrint) . nub . foldDeclared (:) []) d

newHeader f modules _ _ = pure ()

-- | Find the declaration of the export spec in the current module.
-- If it is not there, it is a re-export which we just keep.
doExport :: MoveSpec -> ModuleInfo -> A.ExportSpec SrcSpanInfo -> RWS String String S ()
doExport f info@(ModuleInfo {_moduleKey = k}) spec =
    case findDeclOfExportSpec info spec of
      Nothing -> (tell' . endLoc . A.ann) spec
      Just d -> case f k d of
                  k' | k' == k -> (tell' . endLoc . A.ann) spec
                  _ -> point .= (endLoc . A.ann) spec
    where
      -- Find the declaration that causes all the symbols in the
      -- ImportSpec to come into existance.
findDeclOfExportSpec :: ModuleInfo -> A.ExportSpec SrcSpanInfo -> Maybe (A.Decl SrcSpanInfo)
findDeclOfExportSpec info spec =
    findDeclOfSymbols info (foldDeclared Set.insert mempty spec)
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

{-
    fold $
    foldHeader echo2 echo
               -- Output the module name, which may be different from
               -- the one in the parsed output if we are using another
               -- module as a template for a newly created one.
               (\ _n pref _ suff r -> r |> pref <> prettyPrint (_moduleName (_moduleKey info)) <> suff)
               echo info mempty
-}

skip :: SrcLoc -> RWS String String S ()
skip loc = point .= loc

-- Find the declaration of the symbols of an import spec.  If that
-- declaration moved, update the module name.
newImports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> [A.ImportDecl SrcSpanInfo] -> RWS String String S ()
newImports f modules thismodule imports = do
  mapM_ doImportDecl imports
    where
      doImportDecl x@(A.ImportDecl {importSpecs = Nothing}) = (tell' . endLoc . A.ann) x
      doImportDecl x@(A.ImportDecl {importModule = name, importSpecs = Just (A.ImportSpecList l hiding specs)}) =
          do let oldModname = sModuleName name
             -- Build a map from module key to set of specs to be
             -- imported from it.
             let mp :: Map S.ModuleName (Set (A.ImportSpec SrcSpanInfo))
                 mp = Map.fromListWith Set.union (mapMaybe g (zip (map (newModuleKey oldModname) specs) (map singleton specs)))
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

      -- Given in import spec and the name of the module it was
      -- imported from, return the key of the new module where it will
      -- now be imported from.
      newModuleKey :: S.ModuleName -> A.ImportSpec SrcSpanInfo -> Maybe S.ModuleName
      newModuleKey oldModname spec =
          case findModuleByName oldModname of
            Just info -> case findDeclOfImportSpec info spec of
                           Just d -> Just (_moduleName (f (_moduleKey info) d))
                           -- Everything else we can leave alone - even if we can't
                           -- find a declaration, they might be re-exported.
                           Nothing {- | isReexport info spec -} -> Just oldModname
                           -- Nothing -> trace ("Unable to find decl of " ++ prettyPrint' spec ++ " in " ++ show oldModname) Nothing
            -- If we don't know about the module leave the import spec alone
            Nothing -> Just oldModname

      findModuleByName :: S.ModuleName -> Maybe ModuleInfo
      findModuleByName oldModname =
          case filter (testModuleName oldModname) modules of
            [m] -> Just m
            [] -> Nothing
            ms -> error $ "Multiple " ++ show oldModname

      testModuleName :: S.ModuleName -> ModuleInfo -> Bool
      testModuleName modName info@(ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ name _ _)) _ _ _}) =
          sModuleName name == modName
      testModuleName modName info@(ModuleInfo {_module = A.Module _ Nothing _ _ _}) =
          modName == S.ModuleName "Main"

      -- Find the declaration that causes all the symbols in the
      -- ImportSpec to come into existance.
      findDeclOfImportSpec :: ModuleInfo -> A.ImportSpec SrcSpanInfo -> Maybe (A.Decl SrcSpanInfo)
      findDeclOfImportSpec info spec = findDeclOfSymbols info (foldDeclared Set.insert mempty spec)
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

{-
doImportDecl :: MoveSpec -> [ModuleInfo] -> String -> A.ImportDecl SrcSpanInfo -> String
doImportDecl f modules t x@(A.ImportDecl {..}) =
  snd $ evalRWS (do everywhereM' f x
                    p <- use point
                    t <- view id
                    tell (spanText (p, endLoc x) t))
                t (S {_point = srcLoc (A.ann x), _hiding = True, _subterms = [], _importModule = importModule })
    where
      f :: GenericM (RWS String String S)
      f = mkM doImportSpecs `extM` doImportSpec
      doImportSpecs :: A.ImportSpecList SrcSpanInfo -> RWS String String S (A.ImportSpecList SrcSpanInfo)
      doImportSpecs x@(A.ImportSpecList _ h specs) =
          do t <- view id
             hiding .= h
             pure x
      doImportSpec :: A.ImportSpec SrcSpanInfo -> RWS String String S (A.ImportSpec SrcSpanInfo)
      doImportSpec x@(A.IVar l name) =
          do t <- view id
             subterms %= (++ [spanText x t])
             h <- use hiding
             p <- use point
             tell $ spanText (p, endLoc x) t
             point .= endLoc x
             pure x
      doImportSpec x@(A.IAbs l namespace name) =
          do t <- view id
             subterms %= (++ [spanText x t])
             h <- use hiding
             p <- use point
             tell $ spanText (p, endLoc x) t
             point .= endLoc x
             pure x
      doImportSpec x@(A.IThingAll l name) =
          do t <- view id
             subterms %= (++ [spanText x t])
             h <- use hiding
             p <- use point
             tell $ spanText (p, endLoc x) t
             point .= endLoc x
             pure x
      doImportSpec x@(A.IThingWith l name cnames) =
          do t <- view id
             subterms %= (++ [spanText x t])
             h <- use hiding
             p <- use point
             tell $ spanText (p, endLoc x) t
             point .= endLoc x
             pure x
-}

-- | Remove any exports of symbols that moved elsewhere, add exports
-- of symbols that moved to here, unless moving them here makes them
-- internal.  A symbol is internal if no imports of it exist in the
-- moduverse.
{-
newExports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
newExports f modules info =
    fold (foldExports echo2 (\e pref s suff r -> r |> pref <> s <> suff) echo2 info mempty)
-}

-- | Look through a module's imports, using findDeclOfImportSpec and f
-- to determine which refer to symbols that are moving from one module
-- to another.  There are three cases for an import that moves.  It
-- might move from another module to this module, in which case it can
-- be removed.  It might move between two modules other than this one,
-- in which case the a new import with the new module name is added.
-- The final case is invalid - a module that imported itself.

-- | Monadic variation on everywhere'
everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x
  = do x' <- f x
       gmapM (everywhereM' f) x'

newDecls :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> [A.Decl SrcSpanInfo] -> RWS String String S ()
newDecls f modules info decls = do
  oldDecls
  newDecls
    where
      -- Declarations that were already here and are to remain
      oldDecls :: RWS String String S ()
      oldDecls = mapM_ (\d -> case f (_moduleKey info) d of
                                k | k == _moduleKey info -> tell' (endLoc d)
                                _ -> point .= endLoc d) decls
      -- Declarations that are moving here from other modules.
      -- We have to scan all the modules we know about for this.
      newDecls :: RWS String String S ()
      newDecls = mapM_ (\m@(ModuleInfo {_module = A.Module mspan _ _ _ decls}) ->
                            mapM_ (\d -> case f (_moduleKey m) d of
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

#if 0
-- | Given an ImportSpec, return a map from symbol names to the module
-- and declaration where it is declared.
findDeclOfImportSpec :: [ModuleInfo] -> A.ImportDecl SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Map S.Name (Set (ModuleKey, A.Decl SrcSpanInfo))
findDeclOfImportSpec modules idecl ispec =
    foldl' (\mp m -> foldDecls (\d _ _ _ mp' ->
                                   let declaredSymbols = foldDeclared Set.insert mempty d in
                                   if isSubsetOf importedSymbols declaredSymbols
                                   then foldl' (\mp'' sym -> Map.insertWith union sym (singleton (_moduleKey m, d)) mp'') mp' declaredSymbols
                                   else mp') ignore2 m mp) mempty candidates
    where
      -- All the ModuleInfo values with the required ModuleName (usually there is only one)
      candidates = filter (\info -> _moduleName (_moduleKey info) == sModuleName importedName) modules
      importedSymbols = foldDeclared Set.insert mempty ispec
      importedName = A.importModule idecl

-- | An export spec may refer to symbols declared in the module or
-- (failing that) to symbols imported from another module
-- (re-exports.)  This only finds symbols declared in the current
-- module.
findDeclOfExportSpec :: ModuleInfo -> A.ExportSpec SrcSpanInfo -> Set (A.Decl SrcSpanInfo)
findDeclOfExportSpec info espec =
    foldDecls (\d _ _ _ r ->
                   let declaredSymbols = foldDeclared Set.insert mempty d in
                   if isSubsetOf exportedSymbols declaredSymbols then Set.insert d r else r) ignore2 info mempty
    where
      exportedSymbols = foldDeclared Set.insert mempty espec
#endif
