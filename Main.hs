{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException)
import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Imports (cleanImports)
import System.Directory (getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.FilePath.Find ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import qualified System.IO.Temp as Temp (createTempDirectory)
import Types (loadModule, ModuleInfo(..), ModuleKey(..))
import Decls (moveDecls)

main :: IO ()
-- main = testOn "/home/dsf/git/atp-haskell/src" $ cleanImports
main = testOn "/home/dsf/git/atp-haskell/src" $ \scratch modules -> mapM_ (\(m, s) -> if _moduleText m /= s then writeFile (_modulePath (_moduleKey m) ++ ".new") s else pure ()) (moveDecls (\k d -> k) modules)

testOn :: FilePath -> (FilePath -> [ModuleInfo] -> IO ()) -> IO ()
testOn dir action =
    withCurrentDirectory dir $
    withTempDirectory True "." "scratch" $ \scratch -> do
      paths <- find always (extension ==? ".hs" &&? fileType ==? RegularFile) "."
      modules <- mapM (\path -> loadModule path >>= either (\(e :: SomeException) -> error ("Failed to load " ++ path ++ ": " ++ show e)) pure) paths
      action scratch modules

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
