{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Utils where

import Control.Exception (SomeException, throw)
import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad (MonadPlus, msum, when)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bool (bool)
import Data.Generics (Data(gmapM), GenericM, listify, Typeable)
import Data.List (groupBy, intercalate, stripPrefix)
import Data.Sequence (Seq, (|>))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath (splitFileName)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import qualified System.IO.Temp as Temp (createTempDirectory)
import System.Process (readProcess, readProcessWithExitCode)

-- | dropWhile where predicate operates on two list elements.
dropWhile2 :: (a -> Maybe a -> Bool) -> [a] -> [a]
dropWhile2 f (p : q : rs) | f p (Just q) = dropWhile2 f (q : rs)
dropWhile2 f [p] | f p Nothing = []
dropWhile2 _ l = l

-- | Monadic variation on everywhere'
everywhereM' :: Monad m => GenericM m -> GenericM m
everywhereM' f x
  = do x' <- f x
       gmapM (everywhereM' f) x'

-- | Generically find all values of type b in a value of type a
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

-- | Monadic version of Data.Sequence.|>
(|$>) :: Applicative m => m (Seq a) -> m a -> m (Seq a)
(|$>) s x = (|>) <$> s <*> x

-- | Do a hard reset of all the files of the repository containing the
-- working directory.
gitResetHard :: IO ()
gitResetHard = do
  (code, _out, _err) <- readProcessWithExitCode "git" ["reset", "--hard"] ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure _n -> error "gitResetHard"

-- | Do a hard reset of all the files of a subdirectory within a git
-- repository.  (Does this every throw an exception?)
gitResetSubdir :: FilePath -> IO ()
gitResetSubdir dir = do
  (readProcess "git" ["checkout", "--", dir] "" >>
   readProcess "git" ["clean", "-f", dir] "" >> pure ())
  `IO.catch` \(e :: SomeException) -> hPutStrLn stderr ("gitResetSubdir " ++ show dir ++ " failed: " ++ show e) >> throw e

-- | Determine whether the repository containing the working directory
-- is in a clean state.
gitIsClean :: IO Bool
gitIsClean = do
  (code, out, _err) <- readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case code of
    ExitFailure _ -> error "gitCheckClean failure"
    ExitSuccess | all unmodified (lines out) -> pure True
    ExitSuccess -> pure False
    where
      unmodified (a : b : _) = elem a "?! " && elem b "?! "
      unmodified _ = False

withCleanRepo :: IO a -> IO a
withCleanRepo action = gitIsClean >>= bool (error "withCleanRepo: please commit or revert changes") action

-- | Print a very short and readable version for trace output.
class EZPrint a where
    ezPrint :: a -> String

instance EZPrint a => EZPrint [a] where
    ezPrint xs = "[" ++ intercalate ", " (map ezPrint xs) ++ "]"

instance EZPrint S.ModuleName where
    ezPrint (S.ModuleName s) = s

maybeStripPrefix :: Eq a => [a] -> [a] -> [a]
maybeStripPrefix pre lst = maybe lst id (stripPrefix pre lst)

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
  removeFile path `IO.catch` (\e -> if isDoesNotExistError e then return () else ioError e)
  writeFile path ({-trace (path ++ " text: " ++ show text)-} text)
  text' <- readFile path
  when (text /= text') (error $ "Failed to replace " ++ show path)

-- | A version of lines that preserves the presence or absence of a
-- terminating newline
lines' :: String -> [String]
lines' s =
    -- Group characters into strings containing either only newlines or no newlines,
    -- and then transform the newline only strings into empty lines.
    bol (groupBy (\ a b -> a /= '\n' && b /= '\n') s)
    where
      -- If we are at beginning of line and see a newline, insert an empty
      bol ("\n" : xs) = "" : bol xs
      -- If we are at beginning of line and see something else, call end of line
      bol (x : xs) = x : eol xs
      -- If we see EOF at bol insert a trailing empty
      bol [] = [""]
      -- If we are seeking end of line and see a newline, go to beginning of line
      eol ("\n" : xs) = bol xs
      -- This shouldn't happen
      eol (x : xs) = x : eol xs
      eol [] = []
