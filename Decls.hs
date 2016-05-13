{-# LANGUAGE TupleSections #-}
module Decls (moveDecls) where

import Control.Exception (SomeException)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Function (on)
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
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrint, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importLoc, importModule, importSpecs), ModuleName(..), Name(..))
import SrcLoc (srcLoc, spanText)
import Symbols (FoldDeclared(foldDeclared), symbolsDeclaredBy)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode, showCommandForUser)
import Types (DerivDeclTypes(derivDeclTypes), hseExtensions, hsFlags, loadModule,
              ModuleInfo(ModuleInfo, _module, _moduleKey, _moduleText), ModuleKey(_modulePath, _moduleTop, _moduleName))

-- | Specifies where to move each declaration of each module.
type MoveSpec = ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey

-- | Move the declarations in the ModuleInfo list according to the
-- MoveSpec function, adjusting imports and exports as necessary.
moveDecls :: MoveSpec -> [ModuleInfo] -> [(ModuleInfo, String)]
moveDecls f modules = map (\info -> (info, moveDecls' f modules info)) modules

moveDecls' :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
moveDecls' f modules info =
  newHeader f modules info <>
  newExports f modules info <>
  newImports f modules info <>
  newDecls f modules info

newHeader :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
newHeader f modules info =
    fold $
    foldHeader echo2 echo
               -- Output the module name, which may be different from
               -- the one in the parsed output if we are using another
               -- module as a template for a newly created one.
               (\ _n pref _ suff r -> r |> pref <> prettyPrint (_moduleName (_moduleKey info)) <> suff)
               echo info mempty

-- | Remove any exports of symbols that moved elsewhere, add exports
-- of symbols that moved to here, unless moving them here makes them
-- internal.  A symbol is internal if no imports of it exist in the
-- moduverse.
newExports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
newExports f modules info =
    fold (foldExports echo2 (\e pref s suff r -> r |> pref <> s <> suff) echo2 info mempty)

-- | Look through a module's imports, using findDeclOfImportSpec and f
-- to determine which refer to symbols that are moving from one module
-- to another.  There are three cases for an import that moves.  It
-- might move from another module to this module, in which case it can
-- be removed.  It might move between two modules other than this one,
-- in which case the a new import with the new module name is added.
-- The final case is invalid - a module that imported itself.
newImports :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
newImports f modules info =
    fold (foldImports doImportDecl info mempty)
    where
      -- doImportDecl (A.ImportDecl {importModule = m, importSpecs = Just specs}) pref s suff r = r |> pref <> doImportSpecs specs <> suff
      doImportDecl :: A.ImportDecl SrcSpanInfo -> String -> String -> String -> Seq String -> Seq String
      doImportDecl (A.ImportDecl {A.importModule = m, A.importSpecs = Just (A.ImportSpecList _ False specs)}) pref s suff r =
          r |> pref <> concatMap (doImportSpec m) specs <> suff
      doImportDecl (A.ImportDecl {A.importModule = m, A.importSpecs = Just (A.ImportSpecList _ True specs)}) pref s suff r =
          r |> pref <> concatMap (doHidingSpec m) specs <> suff -- hiding
      doImportDecl (A.ImportDecl {A.importModule = m, A.importSpecs = Nothing}) pref s suff r =
          r |> pref <> s <> suff
      doImportSpec :: A.ModuleName SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> String
      doImportSpec modname x@(A.IVar l name) = spanText x (_moduleText info)
      doImportSpec modname x@(A.IAbs l namespace name) = spanText x (_moduleText info)
      doImportSpec modname x@(A.IThingAll l name) = spanText x (_moduleText info)
      doImportSpec modname x@(A.IThingWith l name cnames) = spanText x (_moduleText info)
      doHidingSpec :: A.ModuleName SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> String
      doHidingSpec modname x@(A.IVar l name) = spanText x (_moduleText info)
      doHidingSpec modname x@(A.IAbs l namespace name) = spanText x (_moduleText info)
      doHidingSpec modname x@(A.IThingAll l name) = spanText x (_moduleText info)
      doHidingSpec modname x@(A.IThingWith l name cnames) = spanText x (_moduleText info)

newDecls :: MoveSpec -> [ModuleInfo] -> ModuleInfo -> String
newDecls f modules info =
    oldDecls <> newDecls <> fold (foldDecls (\_ _ _ _ r -> r) (\s r -> r |> s) info mempty)
    where
      testDecls :: ModuleInfo -> String
      testDecls m = fold (foldDecls (\d pref s suff r ->
                                         case f (_moduleKey m) d of
                                           k | k == _moduleKey info -> r |> pref <> s <> suff
                                           _ -> r) ignore2 m mempty)
      oldDecls = testDecls info
      newDecls = concat (map testDecls (filter (\m -> _moduleKey m /= _moduleKey info) modules))

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
