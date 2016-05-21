-- | Test the following cases for import moving
-- (1) Import of a symbol moves out of a module
-- (2) Import of a symbol that moves between two other modules
-- (3a) Import of a symbol that moves into a module, but is
--      still used by its old module
-- (3b) Import of a symbol that moves into a module, but is
--      no longer used by its old module

module DeclTests where

import Debug.Trace
import Control.Exception (SomeException)
import Data.List hiding (find)
import Data.Maybe
import Data.Set as Set (Set, insert, member)
import Decls (moveDecls, moveDeclsAndClean)
import IO (withCurrentDirectory, withTempDirectory)
import qualified Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Annotated.Simplify (sName)
import qualified Language.Haskell.Exts.Syntax as S
import Symbols (foldDeclared)
import System.FilePath.Find ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit
import Types
import Utils (gitResetSubdir)

declTests :: Test
declTests = TestList [ decl1 ]

decl1 :: Test
decl1 =
    TestCase $ do
      gitResetSubdir input
      withCurrentDirectory input $
        withTempDirectory True "." "scratch" $ \scratch -> do
          paths <- (catMaybes . map (stripPrefix "./")) <$> (find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
          loadModules paths >>= moveDeclsAndClean moveSpec1 scratch
      (_, diff, _) <- readProcessWithExitCode "diff" ["-ru", expected, input] ""
      gitResetSubdir input
      assertString diff
    where
      input = "tests/input/atp-haskell"
      expected = "tests/expected/decl1"

moveSpec1 :: ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey
moveSpec1 k (A.TypeSig _ [A.Ident _ s] _)
    | s == "tryfindM" {-|| s == "failing"-} =
        k {_moduleName = Just (S.ModuleName "Data.Logic.ATP.Tableaux")}
moveSpec1 k (A.FunBind _ ms)
    | any (`elem` [S.Ident "tryfindM" {-, S.Ident "failing"-}])
          (map (\match -> case match of
                            A.Match _ name _ _ _ -> sName name
                            A.InfixMatch _ _ name _ _ _ -> sName name) ms) =
                                     k {_moduleName = Just (S.ModuleName "Data.Logic.ATP.Tableaux")}
{-
moveSpec1 k d | Set.member (S.Ident "tryfindM") (foldDeclared Set.insert mempty d) =
                  trace ("Expected TypeSig or FunBind: " ++ show d)
                        (k {_modulePath = "Data/Logic/ATP/Tableaux.hs",
                            _moduleName = S.ModuleName "Data.Logic.ATP.Tableaux"})
moveSpec1 k d@(A.FunBind _ _) = {-trace ("FunBind: " ++ show (foldDeclared (:) mempty d))-} k
moveSpec1 k (A.ClassDecl _ _mcxt _dh _fd _mcds) = k
moveSpec1 k (A.DataDecl _ _dn _mcxt _dh _qcs _md) = k
moveSpec1 k (A.PatBind _ _p _rhs _mbs) = k
moveSpec1 k (A.TypeDecl _ _dh _typ) = k
moveSpec1 k (A.TypeSig _ _name _typ) = k
moveSpec1 k (A.InstDecl _ _mo _ir _mids) = k
moveSpec1 k (A.InfixDecl _ _assoc _mi _ops) = k
moveSpec1 k (A.DerivDecl {}) = k
moveSpec1 k d = error $ "Unexpected decl: " ++ take 120 (show d) ++ ".."
-}
moveSpec1 k _ = k
