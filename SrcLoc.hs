{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc
    ( spanInfo
    , srcSpan
    , srcLoc
    , endLoc
    , srcLoc'
    , endLoc'
    , textEndLoc
    , increaseSrcLoc
    , textSpan
    , spanText
    , srcPairText
    , makeTree
    , validateParseResults
    ) where

import Control.Monad (MonadPlus, msum)
import Control.Monad.State (get, put, runState, State)
import Data.Generics (Data, listify, Typeable)
import Data.List (groupBy, partition, sort)
import Data.Monoid ((<>))
import Data.Set (Set, toList)
import Data.Tree (Tree, unfoldTree)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Annotated(ann), Module(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

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

spanInfo :: A.Annotated ast => ast SrcSpanInfo -> SrcSpanInfo
spanInfo = A.ann

srcSpan :: A.Annotated ast => ast SrcSpanInfo -> SrcSpan
srcSpan = srcInfoSpan . spanInfo

srcLoc :: A.Annotated ast => ast SrcSpanInfo -> SrcLoc
srcLoc x = let (SrcSpan f b e _ _) = srcSpan x in SrcLoc f b e

endLoc :: A.Annotated ast => ast SrcSpanInfo -> SrcLoc
endLoc x = let (SrcSpan f _ _ b e) = srcSpan x in SrcLoc f b e

srcLoc' :: SrcSpanInfo -> SrcLoc
srcLoc' x = let (SrcSpan f b e _ _) = srcInfoSpan x in SrcLoc f b e

endLoc' :: SrcSpanInfo -> SrcLoc
endLoc' x = let (SrcSpan f _ _ b e) = srcInfoSpan x in SrcLoc f b e

textEndLoc :: FilePath -> String -> SrcLoc
textEndLoc path x =
    SrcLoc {srcFilename = path, srcLine = length ls, srcColumn = length (last ls) + 1}
    where ls = lines' x

-- | Update a SrcLoc to move it from l past the string argument.
increaseSrcLoc :: String -> SrcLoc -> SrcLoc
increaseSrcLoc "" l = l
increaseSrcLoc ('\n' : s) (SrcLoc f y _) = increaseSrcLoc s (SrcLoc f (y + 1) 1)
increaseSrcLoc (_ : s) (SrcLoc f y x) = increaseSrcLoc s (SrcLoc f y (x + 1))

-- | Return a span that exactly covers the string s
textSpan :: FilePath -> String -> SrcSpanInfo
textSpan path s =
    let end = textEndLoc path s in
    SrcSpanInfo {srcInfoSpan = SrcSpan { srcSpanFilename = path
                                       , srcSpanStartLine = 1
                                       , srcSpanStartColumn = 1
                                       , srcSpanEndLine = srcLine end
                                       , srcSpanEndColumn = srcColumn end },
                 srcInfoPoints = []}

-- | Return the text before and after a location
splitText :: SrcLoc -> String -> (String, String)
splitText l s =
    srcPairText (l {srcLine = 1, srcColumn = 1}) l s

-- | Return the text before, within, and after a span
splitSpan :: SrcLoc -> SrcLoc -> String -> (String, String, String)
splitSpan b e s =
    let (pref, s') = splitText b s in
    let (s'', suff) = srcPairText b e s' in
    (pref, s'', suff)

spanText :: A.Annotated ast => ast SrcSpanInfo -> String -> String
spanText sp t = (\(_, x, _) -> x) (splitSpan (srcLoc sp) (endLoc sp) t) -- t


-- | Given a beginning and end location, and a string which starts at
-- the beginning location, return a (beforeend,afterend) pair.
srcPairText :: SrcLoc -> SrcLoc -> String -> (String, String)
srcPairText b0 e s0 =
    fst $ runState f (b0, "", s0)
    where
      f :: State (SrcLoc, String, String) (String, String)
      f = do (b, r, s) <- get
             case (srcLine b < srcLine e, srcColumn b < srcColumn e) of
               -- We have not reached the last line
               (True, _) ->
                   case span (/= '\n') s of
                     (r', '\n' : s') ->
                         -- We see a newline, move it and everything
                         -- before it from s to r, and move point to
                         -- next line.
                         put (b {srcLine = srcLine b + 1, srcColumn = 1}, r ++ r' ++ "\n", s') >> f
                     (_, "") ->
                        -- This should not happen, but if the last line
                        -- lacks a newline terminator, haskell-src-exts
                        -- will set the end location as if the terminator
                        -- was present.
                        case s of
                          "" -> return (r, s)
                          (c : s') -> put (b {srcColumn = srcColumn b + 1}, r ++ [c], s') >> f
                     _ -> error "Impossible: span stopped at the wrong character"
               (_, True) ->
                   case s of
                     [] -> error $ "srcPairText - short line! expected " ++ show (srcColumn e - srcColumn b) ++ " more characters.\n" ++ show (b0, e, s0)
                     (c : s') -> put (b {srcColumn = srcColumn b + 1}, r ++ [c], s') >> f
               _ ->
                   return (r, s)

-- | Build a tree of SrcSpanInfo
makeTree :: (A.Annotated ast, Show (ast SrcSpanInfo), Eq (ast SrcSpanInfo), Ord (ast SrcSpanInfo)) => Set (ast SrcSpanInfo) -> Tree (ast SrcSpanInfo)
makeTree s =
    case findRoots (toList s) of
      [] -> error "No roots"
      [root] -> unfoldTree f root
      roots -> error $ "Multiple roots: " ++ show roots
    where
      f x = (x, findChildren (toList s) x)

      -- The roots are the nodes that are not covered by any other node.
      findRoots :: (A.Annotated a, Eq (a SrcSpanInfo), Ord (a SrcSpanInfo)) => [a SrcSpanInfo] -> [a SrcSpanInfo]
      findRoots [] = []
      findRoots (x : xs) =
          let (_children, other) = partition (\ y -> x `covers` y) xs
              (ancestors, cousins) = partition (\ y -> x `coveredBy` y) other in
          case ancestors of
            -- If there are no ancestors, x is a root, and there may be other roots among the cousins
            [] -> x : findRoots cousins
            -- If there are ancestors, there must be a root among them, and there still may be roots among the cousins.
            _ -> findRoots (ancestors ++ cousins)

      findChildren :: (A.Annotated a, Eq (a SrcSpanInfo), Ord (a SrcSpanInfo), Show (a SrcSpanInfo)) =>
                      [a SrcSpanInfo] -> a SrcSpanInfo -> [a SrcSpanInfo]
      findChildren u x = findRoots children where children = sort (filter (\ y -> x `covers` y && x /= y) u)

-- True if a covers b
covers :: (A.Annotated a, A.Annotated b) => a SrcSpanInfo -> b SrcSpanInfo -> Bool
covers a b = srcLoc a <= srcLoc b && endLoc b <= endLoc a

-- True if a is covered by b
coveredBy :: (A.Annotated a, A.Annotated b) => a SrcSpanInfo -> b SrcSpanInfo -> Bool
coveredBy = flip covers

{-
test3 = TestCase (assertEqual "covers1" True (covers (mkspan (29, 1) (29, 8)) (mkspan (29, 3) (29, 7))))
test4 = TestCase (assertEqual "covers2" False (covers (mkspan (29, 1) (29, 8)) (mkspan (29, 3) (29, 10))))
test5 = TestCase (assertEqual "roots1"
                                  [sp 5 10, sp 11 18]
                                  (findRoots [sp 5 10,
                                              sp 11 18,
                                              sp 6 7,
                                              sp 8 9,
                                              sp 12 15]))
-}

-- | Make sure every SrcSpan in the parsed module refers to existing
-- text.  They could still be in the wrong places, so this doesn't
-- guarantee the parse is valid, but its a pretty good bet.
validateParseResults :: A.Module SrcSpanInfo -> String -> IO ()
validateParseResults modul t =
#if 1
    undefined
#else
    mapM_ validateSpan (nub (sort (gFind modul :: [SrcSpan])))
    where
      -- validateSpan :: SrcSpan -> IO ()
      validateSpan x =
          let s = srcLoc x
              e = endLoc x in
          putStrLn ("span " ++ prettyShow s ++ "->" ++ prettyShow e ++ "=" ++ show (spanText x t))
#endif

instance Pretty SrcLoc where
    pPrint l = text ("(l" <> show (srcLine l) ++ ",c" ++ show (srcColumn l) ++ ")")

gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)
