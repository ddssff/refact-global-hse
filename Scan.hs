{-# LANGUAGE TemplateHaskell #-}
module Scan(St(St, _point)
    , SpanM
    , keep
    , skip
    , debugRender
    , void
    ) where

import Control.Lens ((.=), makeLenses, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Annotated(ann), Module(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(srcColumn, srcLine), SrcSpanInfo)
import SrcLoc (endLoc, srcLoc, testSpan, textOfSpan)

data St = St {_point :: SrcLoc}

$(makeLenses ''St)

type SpanM = RWS String String St

keep :: SrcLoc -> SpanM ()
keep loc = do
  t <- view id
  p <- use point
  tell (textOfSpan (testSpan "keep" (p, loc)) t)
  point .= {-trace ("keep " ++ show loc)-} loc

skip :: SrcLoc -> SpanM ()
skip loc = do
  p <- use point
  pure $ testSpan "skip" (p, loc)
  point .= {-trace ("skip " ++ show loc)-} loc

debugRender :: A.Module SrcSpanInfo -> String -> String
debugRender m@(A.Module _ mh ps is ds) s =
    snd $ evalRWS render s (St {_point = (srcLoc (A.ann m)) {srcLine = 1, srcColumn = 1}})
    where
      -- Put [] around the spans (and eventually | at the divisions of the point list)
      render :: SpanM ()
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
