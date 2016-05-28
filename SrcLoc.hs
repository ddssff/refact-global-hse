{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc
    ( -- * SpanInfo queries
      srcLoc
    , endLoc
    , point
    -- * Location and span info for a piece of text
    , spanOfText
    , endLocOfText
    -- * Use span info to extract text
    , textTripleOfSpan
    , textOfSpan
    -- * Repair spans that have column set to 0
    , fixSpan
    , testSpan
    -- RWS monad to scan a text file
    , ScanM
    , scanModule
    , keep
    , keepAll
    , skip
    , trailingWhitespace
    , withTrailingWhitespace
    , debugRender
    ) where

import Control.Lens ((.=), makeLenses, makeLensesFor, use, view)
import Control.Monad (when)
import Control.Monad.RWS (ask, evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Monoid ((<>))
import Debug.Trace
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Annotated(ann), Module(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')

data St = St { _point :: SrcLoc -- The current position in the full text
             , _remaining :: String  -- The text remaining after _point
             }

$(makeLenses ''St)
$(makeLensesFor [("srcFilename", "locFilename"),
                 ("srcLine", "locLine"),
                 ("srcColumn", "locColumn")] ''SrcLoc)
$(makeLensesFor [("srcSpanFilename", "spanFilename"),
                 ("srcSpanStartLine", "spanStartLine"),
                 ("srcSpanStartColumn", "spanStartColumn"),
                 ("srcSpanEndLine", "spanEndLine"),
                 ("srcSpanEndColumn", "spanEndColumn")] ''SrcSpan)
$(makeLensesFor [("srcInfoSpan", "infoSpan"),
                 ("srcInfoPoints", "infoPoints")] ''SrcSpanInfo)

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

locDiff :: SrcLoc -> SrcLoc -> SrcLoc
locDiff (SrcLoc file l1 c1) (SrcLoc _ l2 c2) =
    if l1 == l2
    then SrcLoc file 1 (c1 - c2 + 1)
    else SrcLoc file (l1 - l2 + 1) c1

spanDiff :: SpanInfo a => a -> SrcLoc
spanDiff sp = locDiff (endLoc sp) (srcLoc sp)

locSum :: SrcLoc -> SrcLoc -> SrcLoc
locSum (SrcLoc f l1 c1) (SrcLoc _ l2 c2) =
    if l1 == l2
    then SrcLoc f l1 (c2 + c1 - 1)
    else SrcLoc f (l2 + l1 - 1) c2

endLocOfText :: FilePath -> String -> SrcLoc
endLocOfText path x =
    SrcLoc {srcFilename = path, srcLine = length ls, srcColumn = length (last ls) + 1}
    where ls = lines' x

-- | Return a span that exactly covers the string s
spanOfText :: FilePath -> String -> SrcSpanInfo
spanOfText path s =
    let end = endLocOfText path s in
    SrcSpanInfo {srcInfoSpan = mkSrcSpan (SrcLoc path 1 1) (SrcLoc path (srcLine end) (srcColumn end)),
                 srcInfoPoints = []}

-- | Return the text before, within, and after a span
textTripleOfSpan :: SpanInfo a => a -> String -> (String, String, String)
textTripleOfSpan sp s =
    let (pref, s') = splitText (srcLoc sp) s in
    let (s'', suff) = splitText (spanDiff sp) s' in
    (pref, s'', suff)

textOfSpan :: SpanInfo a => a -> String -> String
textOfSpan sp s =
    let (_, s') = splitText (srcLoc sp) s in
    let (s'', _) = splitText (spanDiff sp) s' in
    s''

testSpan :: SpanInfo a => String -> a -> a
testSpan msg sp =
    case (srcLoc sp, endLoc sp) of
      (SrcLoc _ _ c1, SrcLoc _ _ c2) | c1 < 1 || c2 < 1 -> error ("testSpan - " ++ msg)
      _ -> sp

splitText :: SrcLoc -> String -> (String, String)
splitText loc@(SrcLoc _ l0 c0) s0 =
    fst $ runState f (1, 1, "", s0)
    where
      f :: State (Int, Int, String, String) (String, String)
      f = do (l, c, r, s) <- get
             case (compare l l0, compare c c0) of
               (LT, _) ->
                   case span (/= '\n') s of
                     (r', '\n' : s') ->
                         put (l + 1, 1, r ++ r' ++ "\n", s') >> f
                     (_, "") -> case s of
                                  -- This should not happen, but if the last line
                                  -- lacks a newline terminator, haskell-src-exts
                                  -- will set the end location as if the terminator
                                  -- was present.
                                  "" -> pure (r, s)
                                  (ch : s') -> put (l, c + 1, r ++ [ch], s') >> f
               (_, LT) ->
                   case s of
                     [] -> error "splitText"
                     (ch : s') -> put (l, c + 1, r ++ [ch], s') >> f
               (EQ, EQ) -> pure (r, s)
               _ -> error ("splitText - invalid arguments: loc=" ++ show loc ++ ", s=" ++ show s0)

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
          putStrLn ("span " ++ prettyShow s ++ "->" ++ prettyShow e ++ "=" ++ show (textOfSpan x t))
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
      t1 sp' = {-trace ("fixSpan " ++ show (srcInfoSpan sp) ++ " -> " ++ show (srcInfoSpan sp'))-} sp'

type ScanM = RWS () String St

scanModule :: ScanM () -> String -> FilePath -> String
scanModule action s file = snd $ evalRWS action () (St {_point = SrcLoc file 1 1, _remaining = s})

instance EZPrint SrcLoc where
    ezPrint = prettyShow

keep :: SrcLoc -> ScanM ()
keep loc = do
  t' <- use remaining
  p <- use point
  let (s', t'') = splitText (locDiff loc p) t'
  tell s'
  remaining .= t''
  point .= {-trace ("keep " ++ show loc)-} loc

keepAll :: ScanM ()
keepAll = do
  p@(SrcLoc file _ _) <- use point
  t <- use remaining
  let e' = locSum p (endLocOfText "" t)
  keep e'

skip :: SrcLoc -> ScanM ()
skip loc = do
  p <- use point
  pure $ testSpan "skip" (p, loc)
  t' <- use remaining
  let (_, t'') = splitText (locDiff loc p) t'
  remaining .= t''
  point .= {-trace ("skip " ++ show loc)-} loc

-- | Given the next SpanInfo after the point, return the trailing
-- whitespace. Assumes the point is at the end of a span.
trailingWhitespace :: SpanInfo a => Maybe a -> ScanM String
trailingWhitespace next = do
  t <- use remaining
  loc@(SrcLoc file _ _) <- use point
  let loc'' = maybe (locSum loc (endLocOfText file t)) srcLoc next
  case loc'' >= loc of
    False -> error "trailingWhitespace"
    True -> do
      let (s', _) = splitText (locDiff loc'' loc) t
      case span (/= '\n') s' of
        (pre, '\n' : _suf) -> pure (pre ++ ['\n'])
        _ -> pure ""

withTrailingWhitespace :: SpanInfo a => (SrcLoc -> ScanM ()) -> Maybe a -> ScanM ()
withTrailingWhitespace fn next = do
  s <- trailingWhitespace next
  p <- use point
  fn (locSum p (endLocOfText (view locFilename p) s))

debugRender :: A.Module SrcSpanInfo -> String -> String
debugRender m@(A.Module _ mh ps is ds) s =
    snd $ evalRWS render () (St {_point = (srcLoc (A.ann m)) {srcLine = 1, srcColumn = 1}, _remaining = s})
    where
      -- Put [] around the spans (and eventually | at the divisions of the point list)
      render :: ScanM ()
      render = do
        tell "["
        mapM_ (\x -> keep (srcLoc (A.ann x)) >> tell "[" >> keep (endLoc (A.ann x)) >> tell "]") ps
        maybe void (\h -> keep (srcLoc (A.ann h)) >> tell "[" >> keep (endLoc (A.ann h)) >> tell "]") mh
        mapM_ (\x -> keep (srcLoc (A.ann x)) >> tell "[" >> keep (endLoc (A.ann x)) >> tell "]") is
        mapM_ (\x -> keep (srcLoc (A.ann x)) >> tell "[" >> keep (endLoc (A.ann x)) >> tell "]") ds
        keep (endLoc (A.ann m))
        tell "]"

void :: Monad m => m ()
void = pure ()
