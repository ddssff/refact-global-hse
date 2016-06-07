{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tmp(withCurrentDirectory
    ) where

import Control.Exception (SomeException)
import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (getCurrentDirectory, setCurrentDirectory)


withCurrentDirectory :: forall m a. (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    liftIO (putStrLn ("cd " ++ path)) >>
    IO.bracket (liftIO getCurrentDirectory >>= \save -> liftIO (setCurrentDirectory path) >> return save)
               (liftIO . setCurrentDirectory)
               (const (action `IO.catch` (\(e :: SomeException) -> liftIO (putStrLn ("in " ++ path)) >> throw e)) :: String -> m a)
               -- (const action `catch` (\e -> liftIO (putStrLn ("in " ++ path) >> throw e)))
