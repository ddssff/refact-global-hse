module Tmp(withCurrentDirectory
    ) where

import Control.Exception.Lifted as IO (bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

withCurrentDirectory :: (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    liftIO (putStrLn ("cd " ++ path)) >>
    IO.bracket (liftIO getCurrentDirectory >>= \save -> liftIO (setCurrentDirectory path) >> return save)
               (\saved -> liftIO $ setCurrentDirectory saved)
               (const action)
