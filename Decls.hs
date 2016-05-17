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
import Data.List (find, groupBy, intercalate, nub, sortBy)
import Data.Map as Map (insertWith, Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>), (<|))
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set, singleton, toList, union, unions)
import qualified Data.Set as Set (map)
import Fold (foldDecls, foldExports, foldHeader, foldImports, echo2, echo, ignore, ignore2)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Simplify as S
import Language.Haskell.Exts.Annotated.ExactPrint (exactPrint)
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrint, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo, SrcLoc(..))
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importLoc, importModule, importSpecs), ModuleName(..), Name(..))
import SrcLoc (srcLoc, endLoc, spanText, textSpan)
import Symbols (FoldDeclared(foldDeclared), symbolsDeclaredBy)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Types (DerivDeclTypes(derivDeclTypes), hseExtensions, hsFlags, loadModule,
              ModuleInfo(..), ModuleKey(_modulePath, _moduleTop, _moduleName))

data S =
    S
    { _hiding :: Bool
    , _subterms :: [String]
    , _point :: SrcLoc }

$(makeLenses ''S)

moveDeclsAndClean :: MoveSpec -> FilePath -> [ModuleInfo] -> IO ()
moveDeclsAndClean f scratch modules =
    mapM_ (\(m, s) -> if _moduleText m /= s then writeFile (_modulePath (_moduleKey m) ++ ".new") s else pure ())
          (moveDecls f modules)

-- | Specifies where to move each declaration of each module.
type MoveSpec = ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey

-- | Append an ImportSpec into to the selected ImportDecl(s) and
-- adjust the span info.
{-
addImportSpec :: (A.ImportDecl SrcSpanInfo -> Bool) -> S.ImportSpec SrcSpanInfo -> A.Module SrcSpanInfo -> A.Module SrcSpanInfo
addImportSpec pred spec mdl =
    undefined
-}

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: MoveSpec -> [ModuleInfo] -> [(ModuleInfo, String)]
moveDecls f modules = map (\info -> (info, moveDecls' f modules info)) modules

-- Update one module
moveDecls' :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
moveDecls' f modules info@(ModuleInfo {_module = A.Module l h _ i d}) =
    snd $ evalRWS (do tell' (srcLoc l)
                      newHeader f modules h
                      newImports f modules i
                      newDecls f modules info d
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

-- | Write the new export list.
newHeader :: MoveSpec -> [ModuleInfo] -> Maybe (A.ModuleHead SrcSpanInfo) -> RWS String String S ()
newHeader f modules (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList l specs)))) = do
  tell' (srcLoc l) -- write everything to beginning of first export
  mapM_ doExport specs
    where
      doExport :: A.ExportSpec SrcSpanInfo -> RWS String String S ()
      doExport x = (tell' . endLoc . A.ann) x
newHeader f modules _ = pure ()
{-
    fold $
    foldHeader echo2 echo
               -- Output the module name, which may be different from
               -- the one in the parsed output if we are using another
               -- module as a template for a newly created one.
               (\ _n pref _ suff r -> r |> pref <> prettyPrint (_moduleName (_moduleKey info)) <> suff)
               echo info mempty
-}

newImports :: MoveSpec -> [ModuleInfo] -> [A.ImportDecl SrcSpanInfo] -> RWS String String S ()
newImports f modules imports =
    mapM_ (\x -> (tell' . srcLoc . A.ann) x) imports
    -- concatMap (doImportDecl f modules (_moduleText info)) imports
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
    -- fold (foldDecls (\_ _ _ _ r -> r) (\s r -> r |> s) info mempty)
    where
      testDecls :: ModuleInfo -> String
      testDecls m = fold (foldDecls (\d pref s suff r ->
                                         case f (_moduleKey m) d of
                                           k | k == _moduleKey info -> r |> pref <> s <> suff
                                           _ -> r) ignore2 m mempty)
      -- Declarations that were already here and are to remain
      oldDecls :: RWS String String S ()
      oldDecls = mapM_ (\d -> case f (_moduleKey info) d of
                                k | k == _moduleKey info -> tell' (endLoc d)
                                _ -> point .= endLoc d) decls
      -- Declarations that are moving here from other modules.
      newDecls :: RWS String String S ()
      newDecls = pure () -- concat (map testDecls (filter (\m -> _moduleKey m /= _moduleKey info) modules))

-- | Suppose we found an import Find the declaration that created the symbols in a.  Typically
-- the result is applied to the MoveSpec function.
{-
findDecl :: FoldDeclared a => [ModuleInfo] -> ModuleKey -> a -> Maybe (A.Decl SrcSpanInfo)
findDecl modules key a =
    symbolsDeclaredBy a
-}

-- | Given an ImportSpec, return a map from symbol names to the module
-- and declaration where it is declared.
findDeclOfImportSpec :: [ModuleInfo] -> A.ImportDecl SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Map S.Name (Set (ModuleKey, A.Decl SrcSpanInfo))
findDeclOfImportSpec modules idecl ispec =
    foldr (\m mp -> foldDecls (\d _ _ _ mp' ->
                                   let declaredSymbols = foldDeclared Set.insert mempty d in
                                   if isSubsetOf importedSymbols declaredSymbols
                                   then foldr (\sym mp'' -> Map.insertWith union sym (singleton (_moduleKey m, d)) mp'') mp' declaredSymbols
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
