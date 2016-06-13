{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

import Control.Exception
import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Data.Default
import Data.Generics
import Data.Set as Set (insert)
import Debug.Trace
import Utils (withCurrentDirectory)
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import LoadModule (loadModule)
import ModuleInfo
import SrcLoc
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
  [file] <- getArgs
  m <- loadModule def file
  putStrLn (scanModule (f m) m)
  pure ()
    where
      f :: ModuleInfo SrcSpanInfo -> ScanM ()
      f (ModuleInfo {_module = Module l mh ps is ds}) = do
         mapM_ (decorate "p" . ann) ps
         maybe (pure ()) (decorate "h" . ann) mh
         mapM_ (decorate "i" . ann) is
         mapM_ (decorate "d" . ann) ds
      decorate s x = do
         cs <- use comments
         mapM_ (\(Comment _ sp _) -> keep (srcLoc sp) >> tell "[c|" >> keep (endLoc sp) >> tell "|]") (takeWhile (\(Comment _ sp _) -> srcLoc sp < srcLoc x) cs)
         keep (srcLoc x)
         tell ("[" ++ s ++ "|")
         cs <- use comments
         mapM_ (\(Comment _ sp _) -> keep (srcLoc sp) >> tell "[c|" >> keep (endLoc sp) >> tell "|]") (takeWhile (\(Comment _ sp _) -> srcLoc sp < endLoc x) cs)
         -- mapM_ (keep . srcLoc) (srcInfoPoints $ ann x)
         keep (endLoc x)
         tell "|]"
