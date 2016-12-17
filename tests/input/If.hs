-- | A module with some imports protected by #if.  When the cleaner
-- runs it can only understand one set of CPP variables.  It should
-- not modify imports which it cannot see.  It should clean imports
-- that are inside a combination of #ifs where it can see, leaving them
-- inside the #ifs where they appeared.

{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings #-}

module If where

-- Imports always visible - should be cleaned.  Empty should be removed.
import Data.Text (all, unpack, empty)
import Prelude hiding (all, break, empty)

#if !__GHCJS__
-- imports visible to GHC - should be cleaned and kept inside an ifdef.
-- Break should be kept, drop should be removed.
import Data.Text (break, drop)
#else
-- Imports not visible to GHC - should be left alone even though not used.
import Data.Text (count)
#endif

main = do
#if !__GHCJS__
  putStrLn (unpack (snd (break (== 'a') "abcbabc")))
#endif
  putStrLn (show (all (== 'a') "abcbabc"))
