{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException)
import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Decls ({-moveDecls,-} moveDeclsAndClean)
import IO ()
import Utils (withTempDirectory)
import Utils (withCurrentDirectory)
import Language.Haskell.Exts
import ModuleKey (ModuleKey(..))
import System.Directory (getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.FilePath.Find ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import qualified System.IO.Temp as Temp (createTempDirectory)
import Types (loadModule, ModuleInfo(..))

main :: IO ()
-- main = testOn "/home/dsf/git/atp-haskell/src" $ cleanImports
main = testOn "/home/dsf/git/atp-haskell/src" (moveDeclsAndClean moveSpec)
    where

moveSpec :: ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey
moveSpec k (A.TypeSig _ [A.Ident _ s] _)
    | s == "tryfindM" =
        k {_moduleName = Just (S.ModuleName "Data.Logic.ATP.Tableaux")}
moveSpec k (A.FunBind _ [A.Match _ (A.Ident _ s) _ _ _])
    | s == "tryfindM" =
        k {_moduleName = Just (S.ModuleName "Data.Logic.ATP.Tableaux")}
moveSpec k _d = k

testOn :: FilePath -> (FilePath -> [ModuleInfo] -> IO ()) -> IO ()
testOn dir action =
    withCurrentDirectory dir $
    withTempDirectory True "." "scratch" $ \scratch -> do
      paths <- find always (extension ==? ".hs" &&? fileType ==? RegularFile) "."
      modules <- mapM (\path -> loadModule path >>= either (\(e :: SomeException) -> error ("Failed to load " ++ path ++ ": " ++ show e)) pure) paths
      action scratch modules
