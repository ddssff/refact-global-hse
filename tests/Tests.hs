{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import DeclTests (declTests)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (errors, failures, runTestTT, Test(TestList))

main :: IO ()
main = runTestTT (TestList [declTests]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)
