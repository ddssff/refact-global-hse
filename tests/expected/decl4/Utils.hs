{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
module Utils where

import Control.Exception (SomeException, throw)
import Control.Exception.Lifted as IO (bracket, catch)
import Control.Monad (MonadPlus, msum, when)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
--import Data.Bool (bool)
import Data.Generics (Data(gmapM), GenericM, listify, toConstr, Typeable)
import Data.List (groupBy, intercalate, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Sequence ((|>), Seq)
import qualified Data.Set as Set
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeDirectoryRecursive, removeFile, renameFile, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
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

{-
traceIOException :: MonadIO m => Loc -> m a -> m a
traceIOException loc action =
    action `catch` (\(e :: SomeException) -> liftIO (hPutStrLn stderr ("exception seen at " ++ ++ __LOC ++ ": " ++ show (V e))
-}

-- | Determine whether the repository containing the working directory
-- is in a modified state, if so return the messages.
gitUnclean :: IO (Maybe String)
gitUnclean = do
  here <- getCurrentDirectory
  hPutStrLn stderr ("here: " ++ show here)
  (code, out, _err) <- readProcessWithExitCode "git" ["status", "--porcelain"] ""
  case code of
    ExitFailure _ -> error "gitCheckClean failure"
    ExitSuccess | all unmodified (lines out) -> pure Nothing
    ExitSuccess -> pure $ Just out
    where
      unmodified (a : b : _) = elem a "?! " && elem b "?! "
      unmodified _ = False

gitIsClean :: IO Bool
gitIsClean = maybe True (const False) <$> gitUnclean

withCleanRepo :: IO a -> IO a
withCleanRepo action = do
  gitUnclean >>= maybe action (\s -> error $ "withCleanRepo: please commit or revert changes:\n" ++ s)

-- | Print a very short and readable version for trace output.
class EZPrint a where
    ezPrint :: a -> String

instance EZPrint a => EZPrint [a] where
    ezPrint xs = "[" ++ intercalate ", " (map ezPrint xs) ++ "]"

instance EZPrint (ModuleName ()) where
    ezPrint (ModuleName _ s) = s

instance EZPrint (Maybe (ModuleName ())) where
    ezPrint (Just x) = prettyPrint x
    ezPrint Nothing = "Main"

instance SrcInfo l => EZPrint (InstRule l) where
    ezPrint (IParen _ r) = ezPrint r
    ezPrint (IRule _ _ _ h) = "instance " ++ ezPrint h

instance SrcInfo l => EZPrint (InstHead l) where
    ezPrint (IHParen _ h) = ezPrint h
    ezPrint (IHInfix _ t n) = "(" ++ ezPrint n ++ ") " ++ ezPrint t
    ezPrint (IHCon _ n) = ezPrint n
    ezPrint (IHApp _ h t) = ezPrint h ++ " " ++ ezPrint t

instance EZPrint (QName l) where
    ezPrint = prettyPrint'

instance EZPrint (Name l) where
    ezPrint = prettyPrint'

instance SrcInfo l => EZPrint (Type l) where
    ezPrint = prettyPrint'

instance SrcInfo l => EZPrint (Exp l) where
    ezPrint = prettyPrint'

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
  createDirectoryIfMissing True (takeDirectory path)
  removeFile path `IO.catch` (\e -> if isDoesNotExistError e then return () else ioError e)
  writeFile path text
  -- text' <- readFile path
  -- when (text /= text') (error $ "Failed to replace " ++ show path)

replaceFileWithBackup :: FilePath -> String -> IO ()
replaceFileWithBackup path text = do
  removeIfExists (path ++ "~")
  renameIfExists path (path ++ "~")
  writeFile path text
    where
      removeIfExists x =
          do exists <- doesFileExist x
             when exists (removeFile x)
      renameIfExists src dst =
          do exists <- doesFileExist src
             when exists (System.Directory.renameFile src dst)

-- | Slightly modified lines function from Data.List.  It preserves
-- the presence or absence of a terminating newline by appending [""]
-- if string ends with a newline.  Thus, the corresponding unlines
-- function is intercalate "\n".
lines'                   :: String -> [String]
lines' ""                =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we have
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
lines' s                 =  cons (case break (== '\n') s of
                                    (l, s') -> (l, case s' of
                                                    []      -> [] -- no newline
                                                    _:""    -> [""]
                                                    _:s''   -> lines' s''))
  where
    cons ~(h, t)        =  h : t

listPairs :: [a] -> [(Maybe a, Maybe a)]
listPairs [] = [(Nothing, Nothing)]
listPairs (x : xs) =
    (Nothing, Just x) : listPairs' x xs
    where
      listPairs' x1 (x2 : xs') = (Just x1, Just x2) : listPairs' x2 xs'
      listPairs' x1 [] = [(Just x1, Nothing)]

-- | listTriples [1,2,3,4] ->
--     [(Nothing,1,Just 2),(Just 1,2,Just 3),(Just 2,3,Just 4),(Just 3,4,Nothing)]
listTriples :: [a] -> [(Maybe a, a, Maybe a)]
listTriples l = zip3 ([Nothing] ++ map Just l) l (tail (map Just l ++ [Nothing]))

-- | Like dropWhile, except the last element that satisfied p is included:
--   dropWhileNext even [2,4,6,1,3,5,8] -> [6,1,3,5,8]
dropWhileNext :: (a -> Bool) -> [a] -> [a]
dropWhileNext p xs = mapMaybe fst $ dropWhile (\(_,x) -> maybe True p x) $ listPairs xs

con :: (Typeable a, Data a) => a -> String
con = show . toConstr

prettyPrint' :: Pretty a => a -> String
prettyPrint' = prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode

-- | Part of the notion of a set.  Nothing about elements for now - no
-- singleton, insert, delete.
class SetLike a where
    union :: a -> a -> a
    difference :: a -> a -> a
    intersection :: a -> a -> a
    intersection a b =  difference a (difference a b)

instance Ord a => SetLike (Set.Set a) where
    union = Set.union
    difference = Set.difference

-- | cartesianProduct [[1,2,3], [4,5],[6]] -> [[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = sequence

groupOn :: (a -> Bool) -> [a] -> [[a]]
groupOn p s = filter (\(c : _) -> not (p c)) (groupBy (\a b -> (p a) == (p b)) s)
