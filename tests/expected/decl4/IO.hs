{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module IO
    ( withCurrentDirectory
    , replaceFile
    ) where

import Control.Exception (SomeException)
import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile, setCurrentDirectory)
import System.FilePath (splitFileName)
import System.IO.Error (isDoesNotExistError)

withCurrentDirectory :: forall m a. (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    liftIO (putStrLn ("cd " ++ path)) >>
    IO.bracket (liftIO getCurrentDirectory >>= \save -> liftIO (setCurrentDirectory path) >> return save)
               (liftIO . setCurrentDirectory)
               (const (action `IO.catch` (\(e :: SomeException) -> liftIO (putStrLn ("in " ++ path)) >> throw e)) :: String -> m a)
               -- (const action `catch` (\e -> liftIO (putStrLn ("in " ++ path) >> throw e)))

replaceFile :: FilePath -> String -> IO ()
replaceFile path text = do
  createDirectoryIfMissing True (fst (splitFileName path))
  removeFile path `IO.catch` (\e -> if isDoesNotExistError e then return () else ioError e)
  writeFile path ({-trace (path ++ " text: " ++ show text)-} text)
  text' <- readFile path
  when (text /= text') (error $ "Failed to replace " ++ show path)
