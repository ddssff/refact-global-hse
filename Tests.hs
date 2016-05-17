{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Generics
import Data.Maybe (fromMaybe, listToMaybe)
import Debug.Trace
import GHC.Generics
import IO (withTempDirectory, withCurrentDirectory)
import Language.Haskell.Exts.Annotated
import Types (loadModule, ModuleInfo(..))
import SrcLoc

data St
    = St { _text :: String
         , _point :: SrcLoc
         , _stack :: [SrcSpanInfo] }

$(makeLenses ''St)

-- | Return the text before, within, and after a span
splitSpan :: SrcLoc -> SrcLoc -> String -> (String, String, String)
splitSpan b e s =
    let (pref, s') = splitText b s in
    let (s'', suff) = srcPairText b e s' in
    (pref, s'', suff)

-- | Return the text before and after a location
splitText :: SrcLoc -> String -> (String, String)
splitText l s =
    srcPairText (l {srcLine = 1, srcColumn = 1}) l s

data Info
    = Info
      { srcSpanInfo :: SrcSpanInfo
      , startLoc :: SrcLoc
      , prefix :: String
      , spanText :: String
      , suffix :: String
      } deriving Show

{-
foo :: A.Annotated ast => ast SrcSpanInfo -> String -> ast Info
foo x s = mapM (\y -> everywhere fmap 
-}

test1 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  let m = _module info
  let m' = fmap (f info) m
  putStrLn (show m')
      where
        f :: ModuleInfo -> SrcSpanInfo -> Info
        f info x = let (p, i, s) = splitSpan (srcLoc x) (endLoc x) (_moduleText info) in Info x (getPointLoc x) p i s

test2 :: IO (Module Info)
test2 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  let m = _module info
  traverse (f info) m
    where
      f :: ModuleInfo -> SrcSpanInfo -> IO Info
      f info x = let (p, i, s) = splitSpan (srcLoc x) (endLoc x) (_moduleText info) in pure (Info x (getPointLoc x) p i s)

test3 :: IO (Module Int)
test3 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  evalStateT (traverse (f info) (_module info)) 1
    where
      f :: ModuleInfo -> SrcSpanInfo -> StateT Int IO Int
      f info x@(SrcSpanInfo {srcInfoSpan = sp, srcInfoPoints = pts}) = do
          n <- get
          modify succ
          -- let (p, i, s) = splitSpan (srcLoc x) (endLoc x) (_moduleText info) in
          pure ({-x,-} n)

test4 :: IO (Module (SrcSpan, Int))
test4 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  pure $ evalState (traverse (f info) (_module info)) []
    where
      f :: ModuleInfo -> SrcSpanInfo -> State [SrcSpanInfo] (SrcSpan, Int)
      f info x@(SrcSpanInfo {srcInfoSpan = sp, srcInfoPoints = pts}) = do
          stk <- get
          let stk' = newStack x stk
          put stk'
          return $ (srcInfoSpan x, length stk')

-- | Given a new element with span x, compare it to the span of its
-- parent element on the top of the stack:
--   * if it ends before the parent ends, push it
--   * if it starts after the parent ends, pop the parent (and recurse)
--   * otherwise do nothing (an error I believe)
newStack :: SrcSpanInfo -> [SrcSpanInfo] -> [SrcSpanInfo]
newStack x stk =
    case stk of
      [] -> [x]
      (top : _) | end x <= end top -> (x : stk)
      -- (top : _) | start x < end top -> error "newStack"
      (_ : more) -> newStack x more

data Ann5
    = Ann5 { _sp :: SrcSpanInfo
           , _pre :: String
           , _mid :: String
           , _post :: String } deriving Show

test5 :: IO (Module Ann5)
test5 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  pure $ evalState (traverse (f info) (_module info))
                   (St {_text = _moduleText info,
                        _point = SrcLoc {srcFilename = srcSpanFilename (srcInfoSpan (ann (_module info))), srcLine = 1, srcColumn = 1},
                        _stack = [_moduleSpan info]})
    where
      f :: ModuleInfo -> SrcSpanInfo -> State St Ann5
      f info x@(SrcSpanInfo {srcInfoSpan = sp, srcInfoPoints = pts}) = do
          stack %= popStack x
          stk@(parent : _) <- use stack
          let -- Where does the pre string start?  Examine the the
              -- parent and parent span start points.  Choose the
              -- rightmost point that is still not to the right of the
              -- start of x.
              (lpt : _) = dropWhile (\p -> p > srcLoc x) (reverse (map srcLoc (srcInfoPoints parent)) ++ [srcLoc parent])
              -- Where does the post string end?  Examine the the
              -- parent and parent span end points.  Choose the
              -- leftmost point that is still not to the left of the end
              -- of x.
              (rpt : _) = case dropWhile (\p -> p < endLoc x) (map srcLoc (srcInfoPoints parent) ++ [endLoc parent]) of
                            [] -> error $ "No rpt for " ++ show (endLoc x) ++ " among " ++ show (srcInfoPoints parent ++ [srcInfoSpan parent])
                            xs -> xs
          (pre, text') <- srcPairText <$> pure lpt <*> pure (srcLoc x) <*> use text
          (mid, text'') <- srcPairText <$> pure (srcLoc x) <*> pure (endLoc x) <*> pure text'
          (post, text''') <- srcPairText <$> pure (endLoc x) <*> pure rpt <*> pure text''
          stack %= (x :)
          text .= text'
          point .= srcLoc x
          return $ Ann5 x pre mid post -- (srcInfoSpan x, length stk')

      -- Otherwise pop and repeat
      -- start x is between start top and end top the stack stays the
      -- same.  If start x is past end top we need to pop the stack.
{-
      doStack :: SrcSpanInfo -> State St String
      doStack x = do
        stk <- use stack
        case stk of
          (top : _)
              | end x <= end top -> do
                    s <- movePoint (srcLoc x)
                    stack %= (x:)
                    return s
          (top : more) ->
              do stack .= more
                 movePoint (endLoc top)
                 doStack x
          [] -> error "doStack" -- This shouldn't happen, we start with one element
      movePoint :: SrcLoc -> State St ()
      movePoint p = do
        (_, text') <- srcPairText <$> use point <*> pure p <*> use text
        point .= p
        text .= text'
-}

popStack :: SrcSpanInfo -> [SrcSpanInfo] -> [SrcSpanInfo]
popStack x stk =
    case stk of
      [] -> error "empty stack"
      (top : more) | end x > end top -> popStack x more
      _ -> stk


start :: SrcSpanInfo -> (Int, Int)
start = srcSpanStart . srcInfoSpan

end :: SrcSpanInfo -> (Int, Int)
end = srcSpanEnd . srcInfoSpan

test6 :: IO (Module SrcSpanInfo)
test6 =  do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  evalStateT (everywhereM f (_module info)) (St { _text = _moduleText info
                                                , _point = (srcLoc (ann (_module info))) {srcLine = 1, srcColumn = 1}
                                                , _stack = [] })
    where
      f :: GenericM (StateT St IO)
      f = mkM doModuleHead `extM` doImportSpecs `extM` doImportDecl `extM` doExportSpecList `extM` doExportSpecs
      doImportDecl :: ImportDecl SrcSpanInfo -> StateT St IO (ImportDecl SrcSpanInfo)
      doImportDecl x@(ImportDecl{..}) = pure x
      doImportSpecs :: [ImportSpec SrcSpanInfo] -> StateT St IO [ImportSpec SrcSpanInfo]
      doImportSpecs x = trace ("g: " ++ show x) (pure x)
      doModuleHead :: ModuleHead SrcSpanInfo -> StateT St IO (ModuleHead SrcSpanInfo)
      doModuleHead x = pure x
      doExportSpecList :: ExportSpecList SrcSpanInfo -> StateT St IO (ExportSpecList SrcSpanInfo)
      doExportSpecList x = trace ("h: " ++ show x) (pure x)
      doExportSpecs :: [ExportSpec SrcSpanInfo] -> StateT St IO [ExportSpec SrcSpanInfo]
      doExportSpecs x = trace ("h: " ++ show x) (pure x)

test7 :: IO ()
test7 = do
  Right info <- withCurrentDirectory "../atp-haskell/src" $ loadModule "Data/Logic/ATP/Lib.hs" :: IO (Either SomeException ModuleInfo)
  putStrLn . show . filter (\x -> srcSpanEndColumn x == 0) $ (gFind (_module info) :: [SrcSpan])
