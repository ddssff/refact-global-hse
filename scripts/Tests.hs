{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Generics
import Data.Set as Set (insert)
import Debug.Trace
import Language.Haskell.Exts
import Types (loadModule, ModuleInfo(..))
import SrcLoc
import Symbols (foldDeclared)
import System.Exit (ExitCode(..), exitWith)
import DeclTests
import Test.HUnit (errors, failures, runTestTT, Test(TestList))
import Utils (withCurrentDirectory)

main :: IO ()
main = runTestTT (TestList [declTests]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)

data St
    = St { _text :: String
         , _point :: SrcLoc
         , _stack :: [SrcSpanInfo] }

$(makeLenses ''St)

data Info
    = Info
      { _srcSpanInfo :: SrcSpanInfo
      , _startLoc :: SrcLoc
      , _prefix :: String
      , _spanText :: String
      , _suffix :: String
      } deriving Show

test1 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  let m = _module info
  let m' = fmap (f info) m
  putStrLn (show m')
      where
        f :: ModuleInfo -> SrcSpanInfo -> Info
        f info x = let (p, i, s) = spanTextTriple (srcLoc x, endLoc x) (_moduleText info) in Info x (getPointLoc x) p i s

test2 :: IO (Module Info)
test2 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  let m = _module info
  traverse (f info) m
    where
      f :: ModuleInfo -> SrcSpanInfo -> IO Info
      f info x = let (p, i, s) = spanTextTriple (srcLoc x, endLoc x) (_moduleText info) in pure (Info x (getPointLoc x) p i s)

test3 :: IO (Module Int)
test3 = do
  Right info <- loadModule "CPP.hs" :: IO (Either SomeException ModuleInfo)
  evalStateT (traverse (f info) (_module info)) 1
    where
      f :: ModuleInfo -> SrcSpanInfo -> StateT Int IO Int
      f info x@(SrcSpanInfo {srcInfoSpan = sp, srcInfoPoints = pts}) = do
          n <- get
          modify succ
          -- let (p, i, s) = spanTextTriple (srcLoc x) (endLoc x) (_moduleText info) in
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
      (top : _) | endLoc x <= endLoc top -> (x : stk)
      (_ : more) -> newStack x more

data Ann5
    = Ann5 { _sp :: SrcSpanInfo
           , _pre :: String
           , _mid :: String
           , _post :: String } deriving Show

{-
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
          (pre, text') <- srcPairText <$> pure (lpt, srcLoc x) <*> use text
          (mid, text'') <- srcPairText <$> pure (srcLoc x, endLoc x) <*> pure text'
          (post, text''') <- srcPairText <$> pure (endLoc x, rpt) <*> pure text''
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
-}

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

-- Tests of foldDeclared
test8a :: IO ()
test8a = do
  let d = ClassDecl
           (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 1 44 72,
                         srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 1 29 6,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 89 29 94,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 5 30 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 5 33 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 5 35 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 5 37 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 5 40 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 5 42 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 5 44 5,
                                          SrcSpan "./Data/Logic/ATP/Formulas.hs" 47 1 47 0]})
           (Just
            (CxTuple
             (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 7 29 70,
                           srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 7 29 8,
                                            SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 22 29 23,
                                            SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 41 29 42,
                                            SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 66 29 67,
                                            SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 68 29 70]})
             [ClassA
               (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 8 29 22, srcInfoPoints = []})
               (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 8 29 14, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 8 29 14, srcInfoPoints = []}) "Pretty"))
               [TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 15 29 22, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 15 29 22, srcInfoPoints = []}) "formula")],
              ClassA
               (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 24 29 41, srcInfoPoints = []})
               (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 24 29 33, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 24 29 33, srcInfoPoints = []}) "HasFixity"))
               [TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 34 29 41, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 34 29 41, srcInfoPoints = []}) "formula")],
              ClassA
               (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 43 29 66, srcInfoPoints = []})
               (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 43 29 49, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 43 29 49, srcInfoPoints = []}) "IsAtom"))
               [TyParen
                (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 50 29 66, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 50 29 51,SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 65 29 66]})
                (TyApp
                 (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 51 29 65, srcInfoPoints = []})
                 (TyCon
                  (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 51 29 57, srcInfoPoints = []})
                  (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 51 29 57, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 51 29 57, srcInfoPoints = []}) "AtomOf")))
                 (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 58 29 65, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 58 29 65, srcInfoPoints = []}) "formula")))]]))
           (DHApp
            (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 71 29 88, srcInfoPoints = []})
            (DHead (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 71 29 80, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 71 29 80, srcInfoPoints = []}) "IsFormula")) -- <- THIS
            (UnkindedVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 81 29 88, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 29 81 29 88, srcInfoPoints = []}) "formula")))
           []
           (Just
            [ClsTyFam
             (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 5 30 24, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 5 30 9]})
             (DHApp
              (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 10 30 24, srcInfoPoints = []})
              (DHead (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 10 30 16, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 10 30 16, srcInfoPoints = []}) "AtomOf")) -- <- THIS
              (UnkindedVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 17 30 24, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 30 17 30 24, srcInfoPoints = []}) "formula")))
             Nothing,
            ClsDecl
             (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 5 33 20, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 10 33 12]})
             (TypeSig
              (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 5 33 20, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 10 33 12]})
              [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 5 33 9, srcInfoPoints = []}) "true"] -- <- THIS
              (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 13 33 20, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 33 13 33 20, srcInfoPoints = []}) "formula"))),
            ClsDecl
             (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 5 35 21, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 11 35 13]})
             (TypeSig
              (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 5 35 21, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 11 35 13]})
              [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 5 35 10, srcInfoPoints = []}) "false"] -- <- THIS
              (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 14 35 21, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 35 14 35 21, srcInfoPoints = []}) "formula"))),
            ClsDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 5 37 36, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 12 37 14]}) (TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 5 37 36, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 12 37 14]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 5 37 11, srcInfoPoints = []}) "asBool"] (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 15 37 36, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 23 37 25]}) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 15 37 22, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 15 37 22, srcInfoPoints = []}) "formula")) (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 26 37 36, srcInfoPoints = []}) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 26 37 31, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 26 37 31, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 26 37 31, srcInfoPoints = []}) "Maybe"))) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 32 37 36, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 32 37 36, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 37 32 37 36, srcInfoPoints = []}) "Bool")))))),
            ClsDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 5 40 40, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 12 40 14]}) (TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 5 40 40, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 12 40 14]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 5 40 11, srcInfoPoints = []}) "atomic"] (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 15 40 40, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 30 40 32]}) (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 15 40 29, srcInfoPoints = []}) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 15 40 21, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 15 40 21, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 15 40 21, srcInfoPoints = []}) "AtomOf"))) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 22 40 29, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 22 40 29, srcInfoPoints = []}) "formula"))) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 33 40 40, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 40 33 40 40, srcInfoPoints = []}) "formula")))),
            ClsDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 5 42 65, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 15 42 17]}) (TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 5 42 65, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 15 42 17]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 5 42 14, srcInfoPoints = []}) "overatoms"] (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 18 42 65, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 45 42 47]}) (TyParen (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 18 42 44, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 18 42 19,SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 43 42 44]}) (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 19 42 43, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 34 42 36]}) (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 19 42 33, srcInfoPoints = []}) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 19 42 25, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 19 42 25, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 19 42 25, srcInfoPoints = []}) "AtomOf"))) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 26 42 33, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 26 42 33, srcInfoPoints = []}) "formula"))) (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 37 42 43, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 39 42 41]}) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 37 42 38, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 37 42 38, srcInfoPoints = []}) "r")) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 42 42 43, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 42 42 43, srcInfoPoints = []}) "r"))))) (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 48 42 65, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 56 42 58]}) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 48 42 55, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 48 42 55, srcInfoPoints = []}) "formula")) (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 59 42 65, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 61 42 63]}) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 59 42 60, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 59 42 60, srcInfoPoints = []}) "r")) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 64 42 65, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 42 64 42 65, srcInfoPoints = []}) "r")))))),
            ClsDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 5 44 72, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 13 44 15]}) (TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 5 44 72, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 13 44 15]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 5 44 12, srcInfoPoints = []}) "onatoms"] (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 16 44 72, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 51 44 53]}) (TyParen (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 16 44 50, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 16 44 17,SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 49 44 50]}) (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 17 44 49, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 32 44 34]}) (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 17 44 31, srcInfoPoints = []}) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 17 44 23, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 17 44 23, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 17 44 23, srcInfoPoints = []}) "AtomOf"))) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 24 44 31, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 24 44 31, srcInfoPoints = []}) "formula"))) (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 35 44 49, srcInfoPoints = []}) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 35 44 41, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 35 44 41, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 35 44 41, srcInfoPoints = []}) "AtomOf"))) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 42 44 49, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 42 44 49, srcInfoPoints = []}) "formula"))))) (TyFun (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 54 44 72, srcInfoPoints = [SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 62 44 64]}) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 54 44 61, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 54 44 61, srcInfoPoints = []}) "formula")) (TyVar (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 65 44 72, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "./Data/Logic/ATP/Formulas.hs" 44 65 44 72, srcInfoPoints = []}) "formula")))))])
  (putStrLn . show) (foldDeclared Set.insert mempty d)

test8b :: IO ()
test8b = do
  let d = IThingWith
            (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 84 32 109, srcInfoPoints = [SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 91 32 92,SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 99 32 100,SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 108 32 109]})
            (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 84 32 91, srcInfoPoints = []}) "Failing")
            [ConName
             (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 92 32 99, srcInfoPoints = []})
             (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 92 32 99, srcInfoPoints = []}) "Success"),
             ConName
             (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 101 32 108, srcInfoPoints = []})
            (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Tableaux.hs" 32 101 32 108, srcInfoPoints = []}) "Failure")]
  (putStrLn . show) (foldDeclared Set.insert mempty d)

test8c :: IO ()
test8c = do
  let d = DataDecl
           (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 67 1 72 18, srcInfoPoints = [SrcSpan "Data/Logic/ATP/Pretty.hs" 68 5 68 6,SrcSpan "Data/Logic/ATP/Pretty.hs" 69 5 69 6,SrcSpan "Data/Logic/ATP/Pretty.hs" 70 5 70 6,SrcSpan "Data/Logic/ATP/Pretty.hs" 71 5 71 6]})
           (DataType (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 67 1 67 5, srcInfoPoints = []}))
           Nothing
           (DHead
            (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 67 6 67 19, srcInfoPoints = []})
            (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 67 6 67 19, srcInfoPoints = []}) "Associativity"))
           [QualConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 68 7 68 13, srcInfoPoints = []})
                        Nothing
                        Nothing
                        (ConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 68 7 68 13, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 68 7 68 13, srcInfoPoints = []}) "InfixL") []),
            QualConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 69 7 69 13, srcInfoPoints = []})
                        Nothing
                        Nothing
                        (ConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 69 7 69 13, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 69 7 69 13, srcInfoPoints = []}) "InfixR") []),
            QualConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 70 7 70 13, srcInfoPoints = []})
                        Nothing
                        Nothing
                        (ConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 70 7 70 13, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 70 7 70 13, srcInfoPoints = []}) "InfixN") []),
            QualConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 71 7 71 13, srcInfoPoints = []})
                        Nothing
                        Nothing
                        (ConDecl (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 71 7 71 13, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 71 7 71 13, srcInfoPoints = []}) "InfixA") [])]
           (Just (Deriving
                  (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 72 5 72 18, srcInfoPoints = [SrcSpan "Data/Logic/ATP/Pretty.hs" 72 5 72 13]})
                  [IRule (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 72 14 72 18, srcInfoPoints = []})
                         Nothing
                         Nothing
                         (IHCon (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 72 14 72 18, srcInfoPoints = []})
                                (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 72 14 72 18, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "Data/Logic/ATP/Pretty.hs" 72 14 72 18, srcInfoPoints = []}) "Show")))]))
  (putStrLn . show) (foldDeclared Set.insert mempty d)
