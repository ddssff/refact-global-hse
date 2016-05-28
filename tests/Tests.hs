{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Data.List (intercalate)
import DeclTests
import Scan (debugRender)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (assertEqual, errors, failures, runTestTT, Test(TestCase, TestList))
import Types (loadModule', ModuleInfo(..))

main :: IO ()
main = runTestTT (TestList [declTests, cpp1]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)

cpp1 :: Test
cpp1 = TestCase $ do
         info@(ModuleInfo {_module = m, _moduleText = s}) <- loadModule' "tests/input/cpp/A.hs"
         assertEqual "cpp1" expected (debugRender m s)
    where
      expected =
          intercalate "\n" ["[[{-# LANGUAGE CPP #-}]",
                            "[module A where]",
                            "",
                            "[import Data.String]",
                            "",
                            "#if 0",
                            "x = \"This is inside #if 0\"",
                            "#else",
                            "[x = \"This is inside the #else\"]",
                            "#endif",
                            "",
                            "[y = \"This is outside the #if\"]",
                            "]"]
