-- | Test the following cases for import moving
-- (1) Import of a symbol moves out of a module
-- (2) Import of a symbol that moves between two other modules
-- (3a) Import of a symbol that moves into a module, but is
--      still used by its old module
-- (3b) Import of a symbol that moves into a module, but is
--      no longer used by its old module

module DeclTests where

import Control.Lens (set, view)
import Data.List hiding (find)
import Data.Maybe
import Decls (appendMoveSpecs, makeMoveSpec, moveDeclsAndClean, MoveSpec)
import IO (withCurrentDirectory, withTempDirectory)
import qualified Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Annotated.Simplify (sName)
import qualified Language.Haskell.Exts.Syntax as S
import ModuleKey (ModuleKey(ModuleKey, _moduleName), moduleName)
import System.FilePath.Find ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import System.Process (readProcessWithExitCode)
import Symbols (foldDeclared)
import Test.HUnit
import Types
import Utils (gitResetSubdir)

declTests :: Test
declTests = TestList [decl1, decl2, decl3, decl4, decl5]

-- Test moving a declaration to a module that currently imports it
decl1 :: Test
decl1 = TestCase $ testMoveSpec "tests/input/atp-haskell" "tests/expected/decl1" moveSpec1

-- Move tryfindM from Lib to Tableaux.  Tableaux already imports
-- tryfindM, so that import should be removed.  The question is, now
-- that tryfindM has moved out of Lib, can we or must we import
-- tryfindM it from Tableaux?  The answer is no, because of this loop:
--
--     Tableaux -> Quantified -> Prop -> Lib -> Tableaux.
--
-- The fact that Tableaux already imports Lib forces us to omit any
-- import of Tableaux from Lib.

moveSpec1 :: ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey
moveSpec1 k@(ModuleKey {_moduleName = n}) (A.TypeSig _ [A.Ident _ s] _)
    | s == "tryfindM" {-|| s == "failing"-} =
        k {_moduleName = S.ModuleName "Data.Logic.ATP.Tableaux"}
moveSpec1 k@(ModuleKey {_moduleName = n}) (A.FunBind _ ms)
    | any (`elem` [S.Ident "tryfindM" {-, S.Ident "failing"-}])
          (map (\match -> case match of
                            A.Match _ name _ _ _ -> sName name
                            A.InfixMatch _ _ name _ _ _ -> sName name) ms) =
        k {_moduleName = S.ModuleName "Data.Logic.ATP.Tableaux"}
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

-- Test moving a declaration to a non-existant module
-- Test updating import of decl that moved from A to B in module C
decl2 :: Test
decl2 = TestCase $ testMoveSpec "tests/input/decl-mover" "tests/expected/decl2" (makeMoveSpec "withCurrentDirectory" "IO" "Tmp")

-- Test moving a declaration to a module that does *not* currently
-- import it.  Now we don't know whether it leaves behind uses for
-- which we need to import it (which might be called "moving down") or
-- if we are moving it to the place where it is used (which could be
-- called "moving up".)
decl3 :: Test
decl3 = TestCase $ testMoveSpec "tests/input/decl-mover" "tests/expected/decl3" (makeMoveSpec "lines'" "SrcLoc" "Utils")

decl4 :: Test
decl4 = TestCase $ testMoveSpec "tests/input/decl-mover" "tests/expected/decl4" spec
    where
      spec = appendMoveSpecs (makeMoveSpec "withTempDirectory" "IO" "Utils") (makeMoveSpec "ignoringIOErrors" "IO" "Utils")

decl5 :: Test
decl5 = TestCase $ testMoveSpec "tests/input/decl-mover" "tests/expected/decl5" spec
    where
      spec = foldl1' appendMoveSpecs [makeMoveSpec "ModuleKey" "Types" "ModuleKey",
                                      makeMoveSpec "fullPathOfModuleKey" "Types" "ModuleKey",
                                      makeMoveSpec "moduleKey" "Types" "ModuleKey"]

decl6 :: Test
decl6 = TestCase $ testMoveSpec "tests/input/decl-mover" "tests/expected/decl6" spec
    where
      spec = foldl1' appendMoveSpecs [makeMoveSpec "MoveSpec" "Decls" "MoveSpec",
                                      makeMoveSpec "makeMoveSpec" "Decls" "MoveSpec",
                                      makeMoveSpec "appendMoveSpecs" "Decls" "MoveSpec",
                                      makeMoveSpec "identityMoveSpec" "Decls" "MoveSpec"]

testMoveSpec :: FilePath -> FilePath -> MoveSpec -> IO ()
testMoveSpec input expected moveSpec = do
  gitResetSubdir input
  withCurrentDirectory input $
    withTempDirectory True "." "scratch" $ \scratch -> do
      paths <- (catMaybes . map (stripPrefix "./")) <$> (find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
      loadModules paths >>= {-mapM (testSpec moveSpec) >>=-} moveDeclsAndClean moveSpec scratch
  (_, diff, _) <- readProcessWithExitCode "diff" ["-ruN", expected, input] ""
  gitResetSubdir input
  assertString diff

testSpec :: MoveSpec -> ModuleInfo -> IO ModuleInfo
testSpec moveSpec m@(ModuleInfo {_moduleKey = k, _module = A.Module _ _ _ _ ds}) = do
  putStrLn ("---- module " ++ show (moduleName k) ++ " ----")
  mapM_ (\d -> let k' = moveSpec k d in
               putStrLn (show (foldDeclared (:) [] d) ++ ": " ++ if k /= k' then show k ++ " " ++  " -> " ++ show k' else "unchanged")) ds
  return m
testSpec _ _ = error "Unexpected module"
