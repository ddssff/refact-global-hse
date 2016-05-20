{-# LANGUAGE RankNTypes #-}
module Utils where

import Control.Monad (MonadPlus, msum)
import Data.Generics (Data(gmapM), GenericM, listify, Typeable)
import Data.Sequence (Seq, (|>))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

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

-- | Do a hard reset of all the files of a subdirectory within a git
-- repository.  (Does this every throw an exception?)
gitResetSubdir :: FilePath -> IO ()
gitResetSubdir dir = do
  (code, _out, _err) <- readProcessWithExitCode "git" ["checkout", "--", dir] ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure _n -> error ("gitResetSubdir " ++ show dir)
