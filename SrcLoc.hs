{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc
    ( -- * SpanInfo queries
      srcLoc
    , endLoc
    -- * Location and span info for a piece of text
    , spanOfText
    , endLocOfText
    -- * Split text at a location
    , splitText
    , splits
    , splits'
    -- * Use span info to extract text
    , textTripleOfSpan
    , textOfSpan
    -- * Repair spans that have column set to 0
    , fixSpan
    , testSpan
    -- RWS monad to scan a text file
    , ScanM
    , point
    , comments
    , remaining
    , scanModule
    , keep
    , keepAll
    , skip
    , fixEnds
    , trailingWhitespace
    , withTrailingWhitespace
    , debugRender
    , mapTopAnnotations
    ) where

import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Debug.Trace
import Language.Haskell.Exts.Annotated.Syntax as A -- (Annotated(ann), Module(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (dropWhileNext, EZPrint(ezPrint), lines')

data St = St { _point :: SrcLoc -- The current position in the full text
             , _remaining :: String  -- The text remaining after _point
             , _comments :: [Comment] -- The comments remaining after _point
             }

$(makeLenses ''St)
$(makeLensesFor [("srcFilename", "locFilename"){-,
                 ("srcLine", "locLine"),
                 ("srcColumn", "locColumn")-}] ''SrcLoc)
$(makeLensesFor [{-("srcSpanFilename", "spanFilename"),
                 ("srcSpanStartLine", "spanStartLine"),
                 ("srcSpanStartColumn", "spanStartColumn"),
                 ("srcSpanEndLine", "spanEndLine"),
                 ("srcSpanEndColumn", "spanEndColumn")-}] ''SrcSpan)
$(makeLensesFor [{-("srcInfoSpan", "infoSpan"),
                 ("srcInfoPoints", "infoPoints")-}] ''SrcSpanInfo)

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

spanDiff :: SrcSpan -> SrcLoc -> SrcSpan
spanDiff sp l = mkSrcSpan (locDiff (srcLoc sp) l) (locDiff (endLoc sp) l)

locSum :: SrcLoc -> SrcLoc -> SrcLoc
locSum (SrcLoc f l1 c1) (SrcLoc _ l2 c2) =
    if l2 == 1
    then SrcLoc f (l1 + l2 - 1) (c1 + c2 - 1)
    else SrcLoc f (l1 + l2 - 1) c2

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
    let (s'', suff) = splitText (locDiff (endLoc sp) (srcLoc sp)) s' in
    (pref, s'', suff)

textOfSpan :: SpanInfo a => a -> String -> String
textOfSpan sp s =
    let (_, s') = splitText (srcLoc sp) s in
    let (s'', _) = splitText (locDiff (endLoc sp) (srcLoc sp)) s' in
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
                     [] -> error ("splitText " ++ ", loc=" ++ show loc ++ ", s=" ++ show s)
                     (ch : s') -> put (l, c + 1, r ++ [ch], s') >> f
               (EQ, EQ) -> pure (r, s)
               _ -> error ("splitText - invalid arguments: loc=" ++ show loc ++ ", s=" ++ show s0)

-- | Using n locations split a string into n + 1 segments.
-- splits (SrcLoc "" 80 20) [SrcLoc "" 80 22, SrcLoc "" 80 25, SrcLoc "" 81 4] "first line\nsecond line" ->
--   [("fi",SrcSpan ""         80 20 80 22),
--    ("rst",SrcSpan ""        80 22 80 25),
--    (" line\nsec",SrcSpan "" 80 25 81  4),
--    ("ond line",SrcSpan ""   81  4 81 12)]
splits :: SrcLoc -> [SrcLoc] -> String -> [(String, SrcSpan)]
splits offset@(SrcLoc file _ _) locs@(_ : locs') s =
    zip (f offset locs s) (map (uncurry mkSrcSpan) (zip (offset : locs) (locs ++ [locSum offset (endLocOfText file s)])))
    where
      f _ [] s = [s]
      f offset (loc : locs) s =
          let (pre, suf) = splitText (locDiff loc offset) s in
          pre : f loc locs suf


data Seg
    = Span (SrcLoc, SrcLoc) String
    | Between (SrcLoc, SrcLoc) String
    deriving Show

-- splits' (SrcLoc "" 80 20) [SrcSpan "" 80 20 80 22, SrcSpan "" 80 25 81 4] "first line\nsecond line"
--   [Span (SrcLoc "" 80 20), "fi", Between (SrcLoc "" 80 22) "rst",
--    Span (SrcLoc "" 80 25 81 4) " line\nsec", Between (SrcLoc "" 81 4) "ond line"]
splits' :: FilePath -> [SrcSpan] -> String -> [Seg]
splits' file spans s =
    f (SrcLoc file 1 1) spans s
    where
      f :: SrcLoc -> [SrcSpan] -> String -> [Seg]
      f offset [] s' = [Between (offset, locSum offset (endLocOfText file s')) s']
      f offset (sp : sps) s''' =
          let (pre, s') = splitText (locDiff (srcLoc sp) offset) s''' in
          let (seg, s'') = splitText (locDiff (endLoc sp) (srcLoc sp)) s' in
          -- trace ("offset=" ++ show offset ++ ", sp=" ++ show sp ++ ", pre=" ++ show pre ++ ", seg=" ++ show seg) $
          (if null pre then [] else [Between (offset, srcLoc sp) pre]) ++ [Span (srcLoc sp, endLoc sp) seg] ++ f (endLoc sp) sps s''
      -- t1 r = trace ("splits' " ++ show file ++ " " ++ show spans ++ " " ++ show s ++ " -> " ++ show r) r
      -- t2 offset el b = trace ("splits' final: offset=" ++ show offset ++ ", el=" ++ show el ++ ", seg=" ++ show b) b

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

scanModule :: ScanM () -> ModuleInfo -> String
scanModule action m@(ModuleInfo {_module = A.Module _ _ _ _ _}) =
    snd $ evalRWS action () (St { _point = SrcLoc (_modulePath m) 1 1
                                , _remaining = _moduleText m
                                , _comments = _moduleComments m })

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
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)

-- | Tighten the start and end points of a span to exclude any leading
-- and trailing whitespace and comments.

-- | Move the endpoint of a span to before any trailing whitespace and comments.
fixEnds :: [Comment] -> String -> SrcSpanInfo -> SrcSpanInfo
fixEnds cs s si@(SrcSpanInfo {srcInfoSpan = sp@(SrcSpan {srcSpanFilename = file})}) =
    let b@(SrcLoc _ bl bc) = realBegin si cs s in
    let e@(SrcLoc _ el ec) = realEnd si cs s in
    case (b < srcLoc sp || b > endLoc sp || e < srcLoc sp || e > endLoc sp) of
      True -> error "fixEnds returned position outside span"
      _ -> si {srcInfoSpan = sp {srcSpanStartLine = bl, srcSpanStartColumn = bc,
                                 srcSpanEndLine = el, srcSpanEndColumn = ec}}

-- | Given a SrcSpanInfo, find the "real" end of the object it covers,
-- meaning the position beyond which lies only whitespace and comments.
realEnd :: SrcSpanInfo -> [Comment] -> String -> SrcLoc
realEnd sp cs s =
  let b@(SrcLoc file _ _) = srcLoc sp
      e = endLoc sp
      s'' = textOfSpan sp s
      commentSpans = map (flip spanDiff b) .
                     takeWhile (\sp -> endLoc sp <= e) .
                     dropWhile (\sp -> srcLoc sp < b) .
                     map (\(Comment _ sp _) -> sp) $ cs
      segs = splits' file commentSpans s'' in
  -- Use the end of the last nonspace segment
  let e' = case dropWhile isWhite (reverse segs) of
            [] -> endLocOfText file s''
            (Span (_, x) _ : _) -> x
            (Between (_, x) _ : _) -> x
      (s''', _) = splitText e' s''
      s'''' = dropWhileEnd isSpace s''' in
      locSum b (endLocOfText file s'''')
      -- e'' = locSum b e' in
  -- if r < b || r > e then error ("realEnd: sp=" ++ show sp ++ ", segs=" ++ show segs ++ " -> " ++ show e'') else e''
    where
      isWhite (Between _ s) | all isSpace s = True
      isWhite (Span _ _) = True
      isWhite _ = False

realBegin :: SrcSpanInfo -> [Comment] -> String -> SrcLoc
realBegin sp cs s =
  let b@(SrcLoc file _ _) = srcLoc sp
      e = endLoc sp
      s'' = textOfSpan sp s
      commentSpans = map (flip spanDiff b) .
                     takeWhile (\sp -> endLoc sp <= e) .
                     dropWhile (\sp -> srcLoc sp < b) .
                     map (\(Comment _ sp _) -> sp) $ cs
      segs = splits' file commentSpans s'' in
  let b' = case dropWhile isWhite segs of
            [] -> b
            (Span (x, _) _ : _) -> {-locSum b-} x
            (Between (x, _) _ : _) -> {-locSum b-} x
      (_, s''') = splitText b' s''
      b'' = endLocOfText "" (takeWhile isSpace s''') in
  foldr1 locSum [b, b', b'']
  -- if r < b || r > e then error ("realEnd: sp=" ++ show sp ++ ", segs=" ++ show segs ++ " -> " ++ show r) else r
    where
      isWhite (Between _ s) | all isSpace s = True
      isWhite (Span _ _) = True
      isWhite _ = False



keepAll :: ScanM ()
keepAll = do
  p@(SrcLoc _ _ _) <- use point
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
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)

-- | Given the next SpanInfo after the point, return the trailing
-- whitespace preceding that span. Assumes the point is at the end of
-- a span.
trailingWhitespace :: Maybe SrcLoc -> ScanM String
trailingWhitespace next = do
  t <- use remaining
  loc@(SrcLoc file _ _) <- use point
  let loc'' = maybe (locSum loc (endLocOfText file t)) id next
  case loc'' >= loc of
    False -> error $ "trailingWhitespace: " ++ show loc'' ++ " < " ++ show loc
    True -> do
      let (s', _) = splitText (locDiff loc'' loc) t
      case span (/= '\n') s' of
        (pre, '\n' : _suf) -> pure (pre ++ ['\n'])
        _ -> pure ""

withTrailingWhitespace :: (SrcLoc -> ScanM ()) -> Maybe SrcLoc -> ScanM ()
withTrailingWhitespace fn next = do
  s <- trailingWhitespace next
  p <- use point
  fn (locSum p (endLocOfText (view locFilename p) s))

debugRender :: A.Module SrcSpanInfo -> [Comment] -> String -> String
debugRender m@(A.Module _ mh ps is ds) cs s =
    snd $ evalRWS render () (St {_point = (srcLoc (A.ann m)) {srcLine = 1, srcColumn = 1}, _comments = cs, _remaining = s})
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

-- | Modify end locations so they precede any trailing whitespace
mapTopAnnotations :: (a -> a) -> A.Module a -> A.Module a
mapTopAnnotations fn (A.Module loc mh ps is ds) =
    A.Module loc (fmap fixMH mh) ps (map fixImport is) (map fixDecl ds)
    where
      fixMH (A.ModuleHead sp name warn specs) = A.ModuleHead (fn sp) name warn specs
      fixImport i = i {importAnn = fn (importAnn i)}
      fixDecl (TypeDecl l a b) = (TypeDecl (fn l) a b)
      fixDecl (TypeFamDecl l a b) = (TypeFamDecl (fn l) a b)
      fixDecl (ClosedTypeFamDecl l a b c) = (ClosedTypeFamDecl (fn l) a b c)
      fixDecl (DataDecl l a b c d e) = (DataDecl (fn l) a b c d e)
      fixDecl (GDataDecl l a b c d e f) = GDataDecl (fn l) a b c d e f
      fixDecl (DataFamDecl l a b c) = (DataFamDecl (fn l) a b c)
      fixDecl (TypeInsDecl l a b) = (TypeInsDecl (fn l) a b)
      fixDecl (DataInsDecl l a b c d) = (DataInsDecl (fn l) a b c d)
      fixDecl (GDataInsDecl l a b c d e) = (GDataInsDecl (fn l) a b c d e)
      fixDecl (ClassDecl l a b c d) = (ClassDecl (fn l) a b c d)
      fixDecl (InstDecl l a b c) = (InstDecl (fn l) a b c)
      fixDecl (DerivDecl l a b) = (DerivDecl (fn l) a b)
      fixDecl (InfixDecl l a b c) = (InfixDecl (fn l) a b c)
      fixDecl (DefaultDecl l a) = (DefaultDecl (fn l) a)
      fixDecl (SpliceDecl l a) = (SpliceDecl (fn l) a)
      fixDecl (TypeSig l a b) = (TypeSig (fn l) a b)
      fixDecl (PatSynSig l a b c d e) = (PatSynSig (fn l) a b c d e)
      fixDecl (FunBind l a) = (FunBind (fn l) a)
      fixDecl (PatBind l a b c) = (PatBind (fn l) a b c)
      fixDecl (PatSyn l a b c) = (PatSyn (fn l) a b c)
      fixDecl (ForImp l a b c d e) = (ForImp (fn l) a b c d e)
      fixDecl (ForExp l a b c d) = (ForExp (fn l) a b c d)
      fixDecl (RulePragmaDecl l a) = (RulePragmaDecl (fn l) a)
      fixDecl (DeprPragmaDecl l a) = (DeprPragmaDecl (fn l) a)
      fixDecl (WarnPragmaDecl l a) = (WarnPragmaDecl (fn l) a)
      fixDecl (InlineSig l a b c) = (InlineSig (fn l) a b c)
      fixDecl (InlineConlikeSig l a b) = (InlineConlikeSig (fn l) a b)
      fixDecl (SpecSig l a b c) = (SpecSig (fn l) a b c)
      fixDecl (SpecInlineSig l a b c d) = (SpecInlineSig (fn l) a b c d)
      fixDecl (InstSig l a) = (InstSig (fn l) a)
      fixDecl (AnnPragma l a) = (AnnPragma (fn l) a)
      fixDecl (MinimalPragma l a) = (MinimalPragma (fn l) a)
      fixDecl (RoleAnnotDecl l a b) = (RoleAnnotDecl (fn l) a b)
