-- | Test the following cases for import moving
-- (1) Import of a symbol moves out of a module
-- (2) Import of a symbol that moves between two other modules
-- (3a) Import of a symbol that moves into a module, but is
--      still used by its old module
-- (3b) Import of a symbol that moves into a module, but is
--      no longer used by its old module

module ImportTests where

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
import qualified Language.Haskell.Exts.Syntax as S
import Symbols (foldDeclared)
import System.FilePath.Find ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit
import Types

declTests :: Test
declTests = TestList [ decl1 ]

decl1 :: Test
decl1 =
    TestCase $ do
      readProcess "git" ["checkout", "--", "tests/input"] ""
      withCurrentDirectory "tests/input" $
        withTempDirectory True "." "scratch" $ \scratch -> do
          paths <- (catMaybes . map (stripPrefix "./")) <$> (find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
          modules <- mapM loadModule' paths
          moveDeclsAndClean f scratch modules
          -- diff <- readProcess "git" ["diff", "Data"] ""
      (_, diff, _) <- readProcessWithExitCode "diff" ["-ru", "tests/expected/decl1", "tests/input"] ""
      readProcess "git" ["checkout", "--", "tests/input"] ""
      assertString diff
    where
      f :: ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey
      f k d | Set.member (S.Ident "tryfindM") (foldDeclared Set.insert mempty d) =
                k {_modulePath = "Data/Logic/ATP/FOL.hs",
                   _moduleName = S.ModuleName "Data.Logic.ATP.FOL"}
      f k (A.TypeSig _ [A.Ident _ s] _)
          | s == "tryfindM" =
              k {_modulePath = "Data/Logic/ATP/FOL.hs",
                 _moduleName = S.ModuleName "Data.Logic.ATP.FOL"}
      f k (A.FunBind _ [A.Match _ (A.Ident _ s) _ _ _])
          | s == "tryfindM" =
              k {_modulePath = "Data/Logic/ATP/FOL.hs",
                 _moduleName = S.ModuleName "Data.Logic.ATP.FOL"}
      f k d@(A.FunBind _ _) = {-trace ("FunBind: " ++ show (foldDeclared (:) mempty d))-} k
      f k (A.ClassDecl _ _mcxt _dh _fd _mcds) = k
      f k (A.DataDecl _ _dn _mcxt _dh _qcs _md) = k
      f k (A.PatBind _ _p _rhs _mbs) = k
      f k (A.TypeDecl _ _dh _typ) = k
      f k (A.TypeSig _ _name _typ) = k
      f k (A.InstDecl _ _mo _ir _mids) = k
      f k (A.InfixDecl _ _assoc _mi _ops) = k
      f k (A.DerivDecl {}) = k
      f k d = error $ "Unexpected decl: " ++ take 120 (show d) ++ ".."

      loadModule' :: FilePath -> IO ModuleInfo
      loadModule' path = either (error . show) id <$> (loadModule path :: IO (Either SomeException ModuleInfo))

      expected =
          unlines
          [ "diff --git a/tests/input/Data/Logic/ATP/FOL.hs b/tests/input/Data/Logic/ATP/FOL.hs"
          , "index dacda7b..75bab27 100644"
          , "--- a/tests/input/Data/Logic/ATP/FOL.hs"
          , "+++ b/tests/input/Data/Logic/ATP/FOL.hs"
          , "@@ -444,4 +444,8 @@ test11 ="
          , " testFOL :: Test"
          , " testFOL = TestLabel \"FOL\" (TestList [test01, test02, test03, test04,"
          , "                                      test05, test06, test07, test08, test09,"
          , "-                                     test10, test11])"
          , "+                                     test10, test11])"
          , " "
          , "+tryfindM :: Monad m => (t -> m (Failing a)) -> [t] -> m (Failing a)"
          , "+tryfindM _ [] = return $ Failure [\"tryfindM\"]"
          , "+"
          , "diff --git a/tests/input/Data/Logic/ATP/Lib.hs b/tests/input/Data/Logic/ATP/Lib.hs"
          , "index a03c306..8b06d85 100644"
          , "--- a/tests/input/Data/Logic/ATP/Lib.hs"
          , "+++ b/tests/input/Data/Logic/ATP/Lib.hs"
          , "@@ -450,10 +450,6 @@ let repetitions ="
          , " tryfind :: Foldable t => (a -> Failing r) -> t a -> Failing r"
          , " tryfind p s = maybe (Failure [\"tryfind\"]) p (find (failing (const False) (const True) . p) s)"
          , " "
          , "-tryfindM :: Monad m => (t -> m (Failing a)) -> [t] -> m (Failing a)"
          , "-tryfindM _ [] = return $ Failure [\"tryfindM\"]"
          , "-tryfindM f (h : t) = f h >>= failing (\\_ -> tryfindM f t) (return . Success)"
          , "-"
          , " evalRS :: RWS r () s a -> r -> s -> a"
          , " evalRS action r s = fst $ evalRWS action r s"
          , " "
          , "diff --git a/tests/input/Data/Logic/ATP/Tableaux.hs b/tests/input/Data/Logic/ATP/Tableaux.hs"
          , "index 185b035..5da45b8 100644"
          , "--- a/tests/input/Data/Logic/ATP/Tableaux.hs"
          , "+++ b/tests/input/Data/Logic/ATP/Tableaux.hs"
          , "@@ -29,7 +29,8 @@ import Data.List as List (map)"
          , " import Data.Logic.ATP.FOL (asubst, fv, generalize, IsFirstOrder, subst)"
          , " import Data.Logic.ATP.Formulas (atomic, IsFormula(asBool, AtomOf), onatoms, overatoms)"
          , " import Data.Logic.ATP.Herbrand (davisputnam)"
          , "-import Data.Logic.ATP.Lib ((|=>), allpairs, deepen, Depth(Depth), distrib, evalRS, Failing(Success, Failure), failing, settryfind, tryfindM)"
          , "+import Data.Logic.ATP.Lib ((|=>), allpairs, deepen, Depth(Depth), distrib, evalRS, Failing(Success, Failure), failing, settryfind)"
          , "+import Data.Logic.ATP.FOL (tryfindM)"
          , " import Data.Logic.ATP.Lit ((.~.), convertToLiteral, IsLiteral, JustLiteral, LFormula, positive)"
          , " import Data.Logic.ATP.LitWrapper (JL)"
          , " import Data.Logic.ATP.Pretty (assertEqual', Pretty(pPrint), prettyShow, text)" ]
