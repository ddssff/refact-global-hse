{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module IO
    ( withTempDirectory
    , replaceFile
    ) where

import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.FilePath (splitFileName)
import System.IO.Error (isDoesNotExistError)
import qualified System.IO.Temp as Temp (createTempDirectory)

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
  removeFile path `IO.catch` (\e -> if isDoesNotExistError e then return () else ioError e)
  writeFile path ({-trace (path ++ " text: " ++ show text)-} text)
  text' <- readFile path
  when (text /= text') (error $ "Failed to replace " ++ show path)
