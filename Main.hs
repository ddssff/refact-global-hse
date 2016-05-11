{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException)
-- import Control.Exception as E (bracket, catch, throw, try)
import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (fold)
import Data.List (groupBy, intercalate, nub)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Sequence ((|>))
import Data.Set as Set (fromList, Set, toList)
import qualified Data.Set as Set (map)
--import GHC.IO.Exception ({-ExitCode(ExitFailure, ExitSuccess),-} IOErrorType(InappropriateType, NoSuchThing), IOException(IOError, ioe_description, ioe_type))
import qualified Language.Haskell.Exts.Annotated as A
import qualified Language.Haskell.Exts.Annotated.CPP as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities) {-, ParseResult-})
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (canonicalizePath, getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>), addExtension, dropExtension, joinPath, splitExtension, splitFileName, splitDirectories)
import System.FilePath.Find ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import System.IO.Error (isDoesNotExistError, isUserError)
import qualified System.IO.Temp as Temp (createTempDirectory)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.HughesPJClass as PP (Pretty(pPrint), prettyShow, text)

import Types
import Imports (cleanImports)
-- import Fold

main :: IO ()
main = withCurrentDirectory "/home/dsf/git/atp-haskell/src" $
       withTempDirectory False "." "scratch" $ \scratch -> do
         paths <- find always (extension ==? ".hs" &&? fileType ==? RegularFile) "."
         modules <- mapM (\path -> loadModule path >>= either (\(e :: SomeException) -> error ("Failed to load " ++ path ++ ": " ++ show e)) pure) paths
         cleanImports scratch modules
         putStrLn "done."

withCurrentDirectory :: (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    liftIO (putStrLn ("cd " ++ path)) >>
    IO.bracket (liftIO getCurrentDirectory >>= \save -> liftIO (setCurrentDirectory path) >> return save)
               (\saved -> liftIO $ setCurrentDirectory saved)
               (const action)

withTempDirectory :: (MonadIO m, MonadBaseControl IO m) =>
                     Bool
                  -> FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory cleanup targetDir template callback =
    IO.bracket
       (liftIO $ Temp.createTempDirectory targetDir template)
       (if cleanup then liftIO . ignoringIOErrors . removeDirectoryRecursive else const (pure ()))
       callback

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `IO.catch` (\e -> const (return ()) (e :: IOError))
