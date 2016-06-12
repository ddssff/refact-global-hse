{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, PackageImports, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc
    ( -- * SpanInfo queries
      srcLoc
    , EndLoc(endLoc)
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
    , locSum
    , locDiff
    , locFilename

    , endOfPragmas
    , endOfHeader
    , endOfImports
    , endOfImportSpecs
    , endOfDecls
    , endOfModule
    , startOfModule
    , startOfPragmas
    , startOfHeader
    , startOfImports
    , startOfDecls
    ) where

import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import "haskell-src-exts-1ast" Language.Haskell.Exts.Syntax -- (Annotated(ann), Module(..))
import "haskell-src-exts-1ast" Language.Haskell.Exts.Comments (Comment(..))
import "haskell-src-exts-1ast" Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')

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

instance Annotated ast => SpanInfo (ast SrcSpanInfo) where
    srcSpan = srcSpan . ann

instance SpanInfo (SrcLoc, SrcLoc) where
    srcSpan (b, e) = mkSrcSpan (SrcLoc (srcFilename b) (srcLine b) (srcColumn b))
                               (SrcLoc (srcFilename e) (srcLine e) (srcColumn e))

srcLoc :: SrcInfo a => a -> SrcLoc
srcLoc = getPointLoc

{-
srcLoc :: SpanInfo a => a -> SrcLoc
srcLoc x = let (SrcSpan f b e _ _) = srcSpan x in SrcLoc f b e
endLoc :: SpanInfo a => a -> SrcLoc
endLoc x = let (SrcSpan f _ _ b e) = srcSpan x in SrcLoc f b e
-}

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
    case ls of
      [] -> SrcLoc {srcFilename = path, srcLine = 1, srcColumn = 1}
      _ -> SrcLoc {srcFilename = path, srcLine = length ls, srcColumn = length (last ls) + 1}
    where ls = lines' x

-- | Return a span that exactly covers the string s
spanOfText :: FilePath -> String -> SrcSpanInfo
spanOfText path s =
    let end = endLocOfText path s in
    SrcSpanInfo {srcInfoSpan = mkSrcSpan (SrcLoc path 1 1) (SrcLoc path (srcLine end) (srcColumn end)),
                 srcInfoPoints = []}

-- | Return the text before, within, and after a span
textTripleOfSpan :: (SrcInfo a, EndLoc a) => a -> String -> (String, String, String)
textTripleOfSpan sp s =
    let (pref, s') = splitText (srcLoc sp) s in
    let (s'', suff) = splitText (locDiff (endLoc sp) (srcLoc sp)) s' in
    (pref, s'', suff)

textOfSpan :: (SrcInfo a, EndLoc a) => a -> String -> String
textOfSpan sp s =
    let (_, s') = splitText (srcLoc sp) s in
    let (s'', _) = splitText (locDiff (endLoc sp) (srcLoc sp)) s' in
    s''

testSpan :: (SrcInfo a, EndLoc a) => String -> a -> a
testSpan msg sp =
    case (srcLoc sp, endLoc sp) of
      (SrcLoc _ l1 c1, SrcLoc _ l2 c2) | c1 < 1 || c2 < 1 || l1 < 1 || l2 < 1 ||
                                         l2 < l1 || (l2 == l1 && c2 < c1) -> error ("testSpan - " ++ msg)
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
                     _ -> error "splitText"
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
splits offset0@(SrcLoc file _ _) locs0@(_ : _) s0 =
    zip (f offset0 locs0 s0) (map (uncurry mkSrcSpan) (zip (offset0 : locs0) (locs0 ++ [locSum offset0 (endLocOfText file s0)])))
    where
      f _ [] s = [s]
      f offset (loc : locs) s =
          let (pre, suf) = splitText (locDiff loc offset) s in
          pre : f loc locs suf
splits (SrcLoc _ _ _) [] _ = error "splits"


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
validateParseResults :: Module SrcSpanInfo -> String -> IO ()
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

instance Pretty SrcSpan where
    pPrint (SrcSpan _ bl bc el ec) = text ("(l" <> show bl ++ ",c" ++ show bc ++ ")->" ++
                                           "(l" <> show el ++ ",c" ++ show ec ++ ")")

instance Pretty SrcSpanInfo where
    pPrint = pPrint . srcInfoSpan

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

scanModule :: ScanM () -> ModuleInfo l -> String
scanModule action m@(ModuleInfo {_module = Module _ _ _ _ _}) =
    snd $ evalRWS action () (St { _point = SrcLoc (_modulePath m) 1 1
                                , _remaining = _moduleText m
                                , _comments = _moduleComments m })
scanModule _ _ = error "scanModule"

instance EZPrint SrcLoc where
    ezPrint = prettyShow

instance EZPrint SrcSpanInfo where
    ezPrint = prettyShow

keep :: SrcLoc -> ScanM ()
keep loc = do
  t' <- use remaining
  p <- use point
  -- pure $ testSpan "keep" (SrcSpan (srcFilename loc) (srcLine p) (srcColumn p) (srcLine loc) (srcColumn loc))
  let (s', t'') = splitText (locDiff loc p) t'
  tell s'
  remaining .= t''
  point .= {-trace ("keep " ++ show loc)-} loc
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)

-- | Tighten the start and end points of a span to exclude any leading
-- and trailing whitespace and comments.

-- | Move the endpoint of a span to before any trailing whitespace and comments.
fixEnds :: [Comment] -> String -> SrcSpanInfo -> SrcSpanInfo
fixEnds cs s si@(SrcSpanInfo {srcInfoSpan = sp}) =
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
                     takeWhile ((<= e) . endLoc) .
                     dropWhile ((< b) . srcLoc) .
                     map (\(Comment _ sp' _) -> sp') $ cs
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
      isWhite (Between _ s') | all isSpace s' = True
      isWhite (Span _ _) = True
      isWhite _ = False

realBegin :: SrcSpanInfo -> [Comment] -> String -> SrcLoc
realBegin sp cs s =
  let b@(SrcLoc file _ _) = srcLoc sp
      e = endLoc sp
      s'' = textOfSpan sp s
      commentSpans = map (flip spanDiff b) .
                     takeWhile ((<= e) . endLoc) .
                     dropWhile ((< b) . srcLoc) .
                     map (\(Comment _ sp' _) -> sp') $ cs
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
      isWhite (Between _ s') | all isSpace s' = True
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
  t' <- use remaining
  p <- use point
  -- pure $ testSpan "skip" (SrcSpan (srcFilename loc) (srcLine p) (srcColumn p) (srcLine loc) (srcColumn loc))
  let (_, t'') = splitText (locDiff loc p) t'
  remaining .= t''
  point .= {-trace ("skip " ++ show loc)-} loc
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)

-- | Assuming the spans of the ast have been adjusted (tightened)
-- using fixEnds, look at the text between point and the beginning of
-- the next span and decide which part belongs to the preceding
-- declaration (or import or whatever) and which belongs to the next
-- one.
trailingWhitespace :: SrcLoc -> ScanM String
trailingWhitespace next = do
  t <- use remaining
  loc <- use point
  case next >= loc of
    False -> error $ "trailingWhitespace: " ++ show next ++ " < " ++ show loc
    True -> do
      let (s', _) = splitText (locDiff next loc) t
      case lines' s' of
        [] -> pure s'
        (x : xs) ->
            -- x is the end of the last line of the declaration (or
            -- whatever), so we always keep that.  Also keep subsequent
            -- nonblank lines.
            case break (all isSpace) xs of
              (_, []) -> pure s'
              (comments', _) -> pure (unlines (x : comments'))

withTrailingWhitespace :: (SrcLoc -> ScanM ()) -> SrcLoc -> ScanM ()
withTrailingWhitespace fn next = do
  s <- trailingWhitespace next
  p <- use point
  fn (locSum p (endLocOfText (view locFilename p) s))

debugRender :: Module SrcSpanInfo -> [Comment] -> String -> String
debugRender m@(Module _ mh ps is ds) cs s =
    snd $ evalRWS render () (St {_point = (srcLoc (ann m)) {srcLine = 1, srcColumn = 1}, _comments = cs, _remaining = s})
    where
      -- Put [] around the spans (and eventually | at the divisions of the point list)
      render :: ScanM ()
      render = do
        tell "["
        mapM_ (\x -> keep (srcLoc (ann x)) >> tell "[" >> keep (endLoc (ann x)) >> tell "]") ps
        maybe void (\h -> keep (srcLoc (ann h)) >> tell "[" >> keep (endLoc (ann h)) >> tell "]") mh
        mapM_ (\x -> keep (srcLoc (ann x)) >> tell "[" >> keep (endLoc (ann x)) >> tell "]") is
        mapM_ (\x -> keep (srcLoc (ann x)) >> tell "[" >> keep (endLoc (ann x)) >> tell "]") ds
        keep (endLoc (ann m))
        tell "]"
debugRender _ _ _ = error "debugRender"

void :: Monad m => m ()
void = pure ()

-- | Modify end locations so they precede any trailing whitespace
mapTopAnnotations :: (a -> a) -> Module a -> Module a
mapTopAnnotations fn (Module loc mh ps is ds) =
    Module loc (fmap fixMH mh) ps (map fixImport is) (map fixDecl ds)
    where
      fixMH (ModuleHead sp name warn specs) = ModuleHead (fn sp) name warn specs
      fixImport i = i {importAnn = fn (importAnn i)}
      fixDecl (TypeDecl l a b) = (TypeDecl (fn l) a b)
      fixDecl (TypeFamDecl l a b c) = (TypeFamDecl (fn l) a b c)
      fixDecl (ClosedTypeFamDecl l a b c d) = (ClosedTypeFamDecl (fn l) a b c d)
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
mapTopAnnotations _ _ = error "mapTopAnnotations"

class EndLoc a where
    endLoc :: a -> SrcLoc
    srcPoints :: a -> [SrcSpan] -- a hack - we should maybe use concrete types?

instance EndLoc SrcSpan where
    endLoc x = SrcLoc (fileName x) (srcSpanEndLine x) (srcSpanEndColumn x)
    srcPoints _ = []
instance EndLoc SrcSpanInfo where
    endLoc = endLoc . srcInfoSpan
    srcPoints = srcInfoPoints
instance EndLoc a => EndLoc (Scoped a) where
    endLoc (Scoped _ x) = endLoc x
    srcPoints (Scoped _ x) = srcPoints x
instance EndLoc (SrcLoc, SrcLoc) where
    endLoc = snd
    srcPoints _ = []

endOfDecls :: EndLoc l => Module l -> SrcLoc
endOfDecls m@(Module _l _mh _ps _ []) = endOfImports m
endOfDecls (Module _l _mh _ps _is ds) = endLoc (ann (last ds))
endOfDecls _ = error "endOfDecls"

endOfImports :: EndLoc l => Module l -> SrcLoc
endOfImports m@(Module _l _mh _ps [] _) = endOfHeader m
endOfImports (Module _l _mh _ps is _) = endLoc (ann (last is))
endOfImports _ = error "endOfImports"

endOfImportSpecs :: (EndLoc l, Show l) => ImportDecl l -> SrcLoc
endOfImportSpecs (ImportDecl {importSpecs = Just i}) =
    case srcPoints (ann i) of
      [] -> error $ "endOfImportSpecs: " ++ show i
      pts -> srcLoc (last pts)
endOfImportSpecs (ImportDecl {importSpecs = Nothing}) = error "endOfImportSpecs"

endOfHeader :: EndLoc l => Module l -> SrcLoc
endOfHeader m@(Module _l Nothing _ps _ _) = endOfPragmas m
endOfHeader (Module _l (Just h) _ps _is _) = endLoc (ann h)
endOfHeader _ = error "endOfHeader"

endOfPragmas :: EndLoc l => Module l -> SrcLoc
endOfPragmas (Module l _ [] _ _) = endLoc l
endOfPragmas (Module _l _ ps _ _) = endLoc (ann (last ps))
endOfPragmas _ = error "endOfPragmas"

endOfModule :: ModuleInfo l -> SrcLoc
endOfModule mi = endLocOfText (_modulePath mi) (_moduleText mi)

startOfModule :: ModuleInfo l -> SrcLoc
startOfModule mi = SrcLoc (_modulePath mi) 1 1

-- | The beginning of the first thing after the imports
startOfDecls :: SrcInfo l => ModuleInfo l -> SrcLoc
startOfDecls mi@(ModuleInfo {_module = Module _l _mh _ps _is []}) = endLocOfText (_modulePath mi) (_moduleText mi)
startOfDecls (ModuleInfo {_module = Module _l _mh _ps _is (d : _)}) = srcLoc (ann d)
startOfDecls _ = error "startOfDecls"

-- | The beginning of the first thing after the header.
startOfImports :: SrcInfo l => ModuleInfo l -> SrcLoc
startOfImports mi@(ModuleInfo {_module = Module _l _mh _ps [] _}) = startOfDecls mi
startOfImports (ModuleInfo {_module = Module _l _mh _ps (i : _) _}) = srcLoc (ann i)
startOfImports _ = error "startOfImports"

-- | The beginning of the first thing after the pragmas.
startOfHeader :: SrcInfo l => ModuleInfo l -> SrcLoc
startOfHeader mi@(ModuleInfo {_module = Module _l Nothing _ps _ _}) = startOfImports mi
startOfHeader (ModuleInfo {_module = Module _l (Just h) _ps _is _}) = srcLoc (ann h)
startOfHeader _ = error "startOfHeader"

-- | The beginning of the first thing
startOfPragmas :: SrcInfo l => ModuleInfo l -> SrcLoc
startOfPragmas (ModuleInfo {_module = m@(Module _l _ [] _ _)}) = SrcLoc (fileName (ann m)) 1 1
startOfPragmas (ModuleInfo {_module = Module _l _ (p : _) _ _}) = srcLoc (ann p)
startOfPragmas _ = error "startOfPragmas"
