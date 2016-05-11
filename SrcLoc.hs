{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc
    ( HasSpanInfo(..)
    , srcSpan
    , srcLoc
    , endLoc
    , textEndLoc
    , increaseSrcLoc
    , textSpan
    , srcPairText
    , makeTree
    , validateParseResults
    ) where

import Control.Monad (MonadPlus, msum)
import Control.Monad.State (get, put, runState, State)
import Data.Generics (Data, listify, Typeable)
import Data.List (groupBy, nub, partition, sort)
import Data.Monoid ((<>))
import Data.Set (Set, toList)
import Data.Tree (Tree(Node), unfoldTree)
import Language.Haskell.Exts.Comments (Comment(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), Module(..), ModuleHead(..), ModuleName(..), ModulePragma(..), WarningText(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Prelude hiding (rem)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)

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

-- type Module = A.Module SrcSpanInfo
type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpecList = A.ExportSpecList SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
-- type ImportSpecList = A.ImportSpecList SrcSpanInfo
-- type ImportSpec = A.ImportSpec SrcSpanInfo
type Decl = A.Decl SrcSpanInfo
-- type QName = A.QName SrcSpanInfo
-- type Name = A.Name SrcSpanInfo
-- type Type = A.Type SrcSpanInfo

class HasSpanInfo a where
    spanInfo :: a -> SrcSpanInfo

instance HasSpanInfo SrcSpan where
    spanInfo x = SrcSpanInfo x []

instance HasSpanInfo a => HasSpanInfo (Tree a) where
    spanInfo (Node x _) = spanInfo x

instance HasSpanInfo ModuleHead where
    spanInfo (A.ModuleHead x _ _ _) = x

instance HasSpanInfo ModuleName where
    spanInfo (A.ModuleName x _) = x

instance HasSpanInfo ModulePragma where
    spanInfo (A.LanguagePragma x _) = x
    spanInfo (A.OptionsPragma x _ _) = x
    spanInfo (A.AnnModulePragma x _) = x

instance HasSpanInfo WarningText where
    spanInfo (A.WarnText x _) = x
    spanInfo (A.DeprText x _) = x

instance HasSpanInfo ExportSpecList where
    spanInfo (A.ExportSpecList x _) = x

instance HasSpanInfo ExportSpec where
    spanInfo (A.EVar x _) = x
    spanInfo (A.EAbs x _ _) = x
    spanInfo (A.EThingAll x _) = x
    spanInfo (A.EThingWith x _ _) = x
    spanInfo (A.EModuleContents x _) = x

instance HasSpanInfo ImportDecl where
    spanInfo (A.ImportDecl x _ _ _ _ _ _ _) = x

instance HasSpanInfo Decl where
    spanInfo (A.TypeDecl l _ _) = l
    spanInfo (A.TypeFamDecl l _ _) = l
    spanInfo (A.DataDecl l _ _ _ _ _) = l
    spanInfo (A.GDataDecl l _ _ _ _ _ _) = l
    spanInfo (A.DataFamDecl l _ _ _) = l
    spanInfo (A.TypeInsDecl l _ _) = l
    spanInfo (A.DataInsDecl l _ _ _ _) = l
    spanInfo (A.GDataInsDecl l _ _ _ _ _) = l
    spanInfo (A.ClassDecl l _ _ _ _) = l
    spanInfo (A.InstDecl l  _ _ _) = l
    spanInfo (A.DerivDecl l _ _) = l
    spanInfo (A.InfixDecl l _ _ _) = l
    spanInfo (A.DefaultDecl l _) = l
    spanInfo (A.SpliceDecl l _) = l
    spanInfo (A.TypeSig l _ _) = l
    spanInfo (A.FunBind l _) = l
    spanInfo (A.PatBind l _ _ _) = l
    spanInfo (A.ForImp l _ _ _ _ _) = l
    spanInfo (A.ForExp l _ _ _ _) = l
    spanInfo (A.RulePragmaDecl l _) = l
    spanInfo (A.DeprPragmaDecl l _) = l
    spanInfo (A.WarnPragmaDecl l _) = l
    spanInfo (A.InlineSig l _ _ _) = l
    spanInfo (A.InlineConlikeSig l _ _) = l
    spanInfo (A.SpecSig l _ _ _) = l
    spanInfo (A.SpecInlineSig l _ _ _ _) = l
    spanInfo (A.InstSig l _) = l
    spanInfo (A.AnnPragma l _) = l
    spanInfo (A.ClosedTypeFamDecl l _ _ _) = l
    spanInfo (A.MinimalPragma l _) = l
    spanInfo (A.PatSynSig l _ _ _ _ _) = l
    spanInfo (A.PatSyn l _ _ _) = l
    spanInfo (A.RoleAnnotDecl l _ _) = l

instance HasSpanInfo SrcSpanInfo where
    spanInfo = id

{-
data SrcSpanInfo
  = SrcSpanInfo { srcInfoSpan :: SrcSpan
                , srcInfoPoints :: [SrcSpan] }

data SrcSpan
  = SrcSpan {srcSpanFilename :: String,
             srcSpanStartLine :: Int,
             srcSpanStartColumn :: Int,
             srcSpanEndLine :: Int,
             srcSpanEndColumn :: Int}

data SrcLoc
  = SrcLoc {srcFilename :: String, srcLine :: Int, srcColumn :: Int}
-}

srcSpan :: HasSpanInfo x => x -> SrcSpan
srcSpan = srcInfoSpan . spanInfo

srcLoc :: HasSpanInfo x => x -> SrcLoc
srcLoc x = let (SrcSpan f b e _ _) = srcSpan x in SrcLoc f b e

endLoc :: HasSpanInfo x => x -> SrcLoc
endLoc x = let (SrcSpan f _ _ b e) = srcSpan x in SrcLoc f b e

textEndLoc :: FilePath -> String -> SrcLoc
textEndLoc path text =
    SrcLoc {srcFilename = path, srcLine = length ls, srcColumn = length (last ls) + 1}
    where ls = lines' text

-- | Update a SrcLoc to move it from l past the string argument.
increaseSrcLoc :: String -> SrcLoc -> SrcLoc
increaseSrcLoc "" l = l
increaseSrcLoc ('\n' : s) (SrcLoc f y _) = increaseSrcLoc s (SrcLoc f (y + 1) 1)
increaseSrcLoc (_ : s) (SrcLoc f y x) = increaseSrcLoc s (SrcLoc f y (x + 1))

-- | Return a span that exactly covers the string s
textSpan :: FilePath -> String -> SrcSpanInfo
textSpan path s =
    let end = textEndLoc path s in
    SrcSpanInfo (SrcSpan {srcSpanFilename = path, srcSpanStartLine = 1, srcSpanStartColumn = 1, srcSpanEndLine = srcLine end, srcSpanEndColumn = srcColumn end}) []

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

spanText :: SrcLoc -> SrcLoc -> String -> String
spanText b e s = (\(_, x, _) -> x) (splitSpan b e s)

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
makeTree :: (HasSpanInfo a, Show a, Eq a, Ord a) => Set a -> Tree a
makeTree s =
    case findRoots (toList s) of
      [] -> error "No roots"
      [root] -> unfoldTree f root
      roots -> error $ "Multiple roots: " ++ show roots
    where
      f x = (x, findChildren (toList s) x)

      -- The roots are the nodes that are not covered by any other node.
      findRoots :: (HasSpanInfo a, Eq a, Ord a) => [a] -> [a]
      findRoots [] = []
      findRoots (x : xs) =
          let (_children, other) = partition (\ y -> x `covers` y) xs
              (ancestors, cousins) = partition (\ y -> x `coveredBy` y) other in
          case ancestors of
            -- If there are no ancestors, x is a root, and there may be other roots among the cousins
            [] -> x : findRoots cousins
            -- If there are ancestors, there must be a root among them, and there still may be roots among the cousins.
            _ -> findRoots (ancestors ++ cousins)

      findChildren :: (HasSpanInfo a, Eq a, Ord a, Show a) => [a] -> a -> [a]
      findChildren u x = findRoots children where children = sort (filter (\ y -> x `covers` y && x /= y) u)

-- True if a covers b
covers :: (HasSpanInfo a, HasSpanInfo b) => a -> b -> Bool
covers a b = srcLoc a <= srcLoc b && endLoc b <= endLoc a

-- True if a is covered by b
coveredBy :: (HasSpanInfo a, HasSpanInfo b) => a -> b -> Bool
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

validateParseResults :: A.Module SrcSpanInfo -> [Comment] -> String -> IO ()
validateParseResults modul comments text =
    mapM_ validateSpan (nub (sort (gFind modul :: [SrcSpan])))
    where
      validateSpan :: SrcSpan -> IO ()
      validateSpan span =
          let s = srcLoc span
              e = endLoc span in
          putStrLn ("span " ++ prettyShow s ++ "->" ++ prettyShow e ++ "=" ++ show (spanText s e text))

instance Pretty SrcLoc where
    pPrint l = text ("(l" <> show (srcLine l) ++ ",c" ++ show (srcColumn l) ++ ")")

gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)
