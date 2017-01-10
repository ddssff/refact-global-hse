-- | RWS monad for scanning haskell modules parsed by haskell-src-exts.

{-# LANGUAGE TemplateHaskell #-}
module Refactor.ScanM
    ( -- RWS monad to scan a text file
      ScanM
    , locFilename
    , point
    , comments
    , remaining
    , scanModule
    , keep
    , keepAll
    , skip
    , trailingWhitespace
    , withTrailingWhitespace
    , debugRender
    ) where

import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Data.Char (isSpace)
-- import Debug.Trace (trace)
import Language.Haskell.Exts.Syntax -- (Annotated(ann), Module(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Refactor.ModuleInfo
import Refactor.SrcLoc
import Refactor.Utils (lines')

type ScanM = RWS () String St

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

scanModule :: ScanM () -> ModuleInfo l -> String
scanModule action m@(ModuleInfo {_module = Module _ _ _ _ _}) =
    snd $ evalRWS action () (St { _point = SrcLoc (_modulePath m) 1 1
                                , _remaining = _moduleText m
                                , _comments = _moduleComments m })
scanModule _ _ = error "scanModule"

-- | Keep everything from the point to loc
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

{-
keepV :: String -> SrcLoc -> ScanM ()
keepV msg loc = do
  t' <- use remaining
  p <- use point
  -- pure $ testSpan "keep" (SrcSpan (srcFilename loc) (srcLine p) (srcColumn p) (srcLine loc) (srcColumn loc))
  let (s', t'') = splitText (locDiff loc p) t'
  tell (trace (msg ++ ": " ++ show s') s')
  remaining .= t''
  point .= {-trace ("keep " ++ show loc)-} loc
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)
-}

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

{-
skipV :: String -> SrcLoc -> ScanM ()
skipV msg loc = do
  t' <- use remaining
  p <- use point
  -- pure $ testSpan "skip" (SrcSpan (srcFilename loc) (srcLine p) (srcColumn p) (srcLine loc) (srcColumn loc))
  let (s, t'') = splitText (locDiff loc p) t'
  trace (msg ++ ": " ++ show s) (pure ())
  remaining .= t''
  point .= {-trace ("skip " ++ show loc)-} loc
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)
-}

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
