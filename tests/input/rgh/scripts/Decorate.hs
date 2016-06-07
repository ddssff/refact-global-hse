{-# LANGUAGE TemplateHaskell #-}
import Control.Exception
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics
import Data.Set as Set (insert)
import Debug.Trace
import Utils (withCurrentDirectory)
import Language.Haskell.Exts.Annotated
import Types (loadModule, loadModule', ModuleInfo(..))
-- import Text.PrettyPrint.HughesPJClass (prettyShow)
import SrcLoc
import Symbols (foldDeclared, toExportSpecs)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (dropExtension)
import System.IO (hPutStrLn, stderr)
import Test.HUnit (errors, failures, runTestTT, Test(TestList))

import SrcLoc
data Params
    = Params { _topDir :: FilePath
             , _paths :: [FilePath]
             }

params0 :: Params
params0 = Params {_topDir = ".", _paths = []}

main = do
  m <- loadModule' "Symbols.hs"
  writeFile "Symbols-Decorated.hs" (scanModule (f m) (_moduleText m) (_moduleComments m) (_modulePath m))
  pure ()
    where
      f (ModuleInfo {_module = Module l mh ps is ds}) = do
         mapM_ decorate ps
         maybe (pure ()) decorate mh
         mapM_ decorate is
         mapM_ decorate ds
      decorate x = do
         cs <- use comments
         mapM_ (\(Comment _ sp _) -> keep (srcLoc sp) >> tell "[c|" >> keep (endLoc sp) >> tell "|]") (takeWhile (\(Comment _ sp _) -> srcLoc sp < srcLoc x) cs)
         keep (srcLoc x)
         tell "[|"
         cs <- use comments
         mapM_ (\(Comment _ sp _) -> keep (srcLoc sp) >> tell "[c|" >> keep (endLoc sp) >> tell "|]") (takeWhile (\(Comment _ sp _) -> srcLoc sp < endLoc x) cs)
         -- mapM_ (keep . srcLoc) (srcInfoPoints $ ann x)
         keep (endLoc x)
         tell "|]"
