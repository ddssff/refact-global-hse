{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc
    ( SpanInfo(srcSpan)
    , srcLoc
    , endLoc
    , textEndLoc
    , increaseSrcLoc
    , textSpan
    , splitText
    , splitSpan
    , spanText
    , srcPairText
    , makeTree
#if 0
    , validateParseResults
#endif
    , fixSpan
    ) where

import Control.Lens (_2, view)
import Control.Monad.State (get, put, runState, State)
import Data.List (groupBy, partition, sort)
import Data.Monoid ((<>))
import Data.Set (Set, toList)
import Data.Tree (Tree, unfoldTree)
import Debug.Trace (trace)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Annotated(ann))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
-- import Utils (gFind)

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

class SpanInfo a where
    srcSpan :: a -> SrcSpan

instance SpanInfo SrcSpan where
    srcSpan = id

instance SpanInfo SrcSpanInfo where
    srcSpan = srcSpan . srcInfoSpan

instance A.Annotated ast => SpanInfo (ast SrcSpanInfo) where
    srcSpan = srcSpan . A.ann

instance SpanInfo (SrcLoc, SrcLoc) where
    srcSpan (b, e) = mkSrcSpan (SrcLoc (srcFilename b) (srcLine b) (srcColumn b))
                               (SrcLoc (srcFilename e) (srcLine e) (srcColumn e))

srcLoc :: SpanInfo a => a -> SrcLoc
srcLoc x = let (SrcSpan f b e _ _) = srcSpan x in SrcLoc f b e
endLoc :: SpanInfo a => a -> SrcLoc
endLoc x = let (SrcSpan f _ _ b e) = srcSpan x in SrcLoc f b e

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
    SrcSpanInfo {srcInfoSpan = mkSrcSpan (SrcLoc path 1 1) (SrcLoc path (srcLine end) (srcColumn end - 1)),
                 srcInfoPoints = []}

-- | Return the text before and after a location
splitText :: SrcLoc -> String -> (String, String)
splitText l s =
    srcPairText (l {srcLine = 1, srcColumn = 1}, l) s

-- | Return the text before, within, and after a span
splitSpan :: SpanInfo a => a -> String -> (String, String, String)
splitSpan sp s =
    let (pref, s') = splitText (srcLoc sp) s in
    let (s'', suff) = srcPairText sp s' in
    (pref, s'', suff)

spanText :: SpanInfo a => a -> String -> String
spanText sp t = view _2 (splitSpan sp t)

-- spanText :: A.Annotated ast => ast SrcSpanInfo -> String -> String
-- spanText sp t = (\(_, x, _) -> x) (splitSpan (srcLoc sp) (endLoc sp) t) -- t


-- | Given a beginning and end location, and a string which starts at
-- the beginning location, return a (beforeend,afterend) pair.
srcPairText :: SpanInfo a => a -> String -> (String, String)
srcPairText sp s0 =
    fst $ runState f (b0, "", s0)
    where
      b0 = srcLoc sp
      e = endLoc sp
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
#if 0
validateParseResults :: A.Module SrcSpanInfo -> String -> IO ()
validateParseResults modul t =
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

-- This happens, a span with end column 0, even though column
-- numbering begins at 1.  Is it a bug in haskell-src-exts?
fixSpan :: SrcSpanInfo -> SrcSpanInfo
fixSpan sp =
    if srcSpanEndColumn (srcInfoSpan sp) == 0
    then t1 $ sp {srcInfoSpan = (srcInfoSpan sp) {srcSpanEndColumn = 1}}
    else sp
    where
      t1 sp' = trace ("fixSpan " ++ show (srcInfoSpan sp) ++ " -> " ++ show (srcInfoSpan sp')) sp'
