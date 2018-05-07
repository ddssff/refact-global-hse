{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Tmp
    ( withCurrentDirectory
    ) where

import Control.Exception (SomeException, throw)
import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

withCurrentDirectory :: forall m a. (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    liftIO (putStrLn ("cd " ++ path)) >>
    IO.bracket acquire release action'
    where
      acquire :: m FilePath
      acquire = liftIO getCurrentDirectory >>= \save -> liftIO (setCurrentDirectory path) >> return save
      release :: FilePath -> m ()
      release = liftIO . setCurrentDirectory
      action' :: FilePath -> m a
      action' _ = action `IO.catch` (\(e :: SomeException) -> liftIO (putStrLn ("in " ++ path)) >> throw e)
