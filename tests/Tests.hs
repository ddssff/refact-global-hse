{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Generics
import Data.Set as Set (insert)
import Debug.Trace
import IO (withCurrentDirectory)
import Language.Haskell.Exts.Annotated
import Types (loadModule, ModuleInfo(..))
-- import Text.PrettyPrint.HughesPJClass (prettyShow)
import SrcLoc
import Symbols (foldDeclared)
import System.Exit (ExitCode(..), exitWith)
import DeclTests
import Test.HUnit (errors, failures, runTestTT, Test(TestList))

main :: IO ()
main = runTestTT (TestList [declTests]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)
