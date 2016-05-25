{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}

module IO
    ( replaceFile
    ) where

import Control.Exception.Lifted as IO (catch)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (splitFileName)
import System.IO.Error (isDoesNotExistError)

replaceFile :: FilePath -> String -> IO ()
replaceFile path text = do
  createDirectoryIfMissing True (fst (splitFileName path))
  removeFile path `IO.catch` (\e -> if isDoesNotExistError e then return () else ioError e)
  writeFile path ({-trace (path ++ " text: " ++ show text)-} text)
  text' <- readFile path
  when (text /= text') (error $ "Failed to replace " ++ show path)
