{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module IO
    ( withTempDirectory
    , withCurrentDirectory
    , replaceFile
    ) where

import Control.Exception (SomeException)
import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.FilePath (splitFileName)
import System.IO.Error
import qualified System.IO.Temp as Temp (createTempDirectory)

withCurrentDirectory :: forall m a. (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    liftIO (putStrLn ("cd " ++ path)) >>
    IO.bracket (liftIO getCurrentDirectory >>= \save -> liftIO (setCurrentDirectory path) >> return save)
               (liftIO . setCurrentDirectory)
               (const (action `IO.catch` (\(e :: SomeException) -> liftIO (putStrLn ("in " ++ path)) >> throw e)) :: String -> m a)
               -- (const action `catch` (\e -> liftIO (putStrLn ("in " ++ path) >> throw e)))

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

replaceFile :: FilePath -> String -> IO ()
replaceFile path text = do
  createDirectoryIfMissing True (fst (splitFileName path))
  removeFile path `catch` (\e -> if isDoesNotExistError e then return () else ioError e)
  writeFile path ({-trace (path ++ " text: " ++ show text)-} text)
  text' <- readFile path
  when (text /= text') (error $ "Failed to replace " ++ show path)
