{-# LANGUAGE FlexibleContexts #-}

module IO
    ( withTempDirectory
    , withCurrentDirectory
    ) where

import Control.Exception (SomeException)
import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Decls ({-moveDecls,-} moveDeclsAndClean)
--import Imports (cleanImports)
import qualified Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S
import System.Directory (getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.FilePath.Find ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import qualified System.IO.Temp as Temp (createTempDirectory)
import Types (loadModule, ModuleInfo(..), ModuleKey(..))

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
