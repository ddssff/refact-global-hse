-- | 'foldModule' is a utility function used to implement the clean, split, and merge operations.
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Fold
    ( (|$>)
    , foldModuleM
    , foldHeaderM
    , foldExportsM
    , foldImportsM
    , foldDeclsM
    , echoM
    , echo2M
    , ignoreM
    , ignore2M
    ) where

import Control.Monad (when)
import Control.Monad.State (get, put, runState, runStateT, State, StateT)
import Control.Monad.Trans (lift)
import Data.Char (isSpace)
import Data.List (tails)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl, ExportSpec, ExportSpec(..), ExportSpecList(ExportSpecList), ImportDecl, Module(..), ModuleHead(..), ModuleName, ModulePragma, WarningText)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
--import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
-- import Language.Haskell.Modules.ModuVerse (ModuleInfo(ModuleInfo))
--import Language.Haskell.Modules.SourceDirs (PathKey)
-- import Language.Haskell.Modules.SrcLoc (endLoc, HasSpanInfo(..), increaseSrcLoc, srcLoc, srcPairText)

import SrcLoc
import Types

-- | Monadic version of Data.Sequence.|>
(|$>) :: Applicative m => m (Seq a) -> m a -> m (Seq a)
(|$>) s x = (|>) <$> s <*> x

--type Module = A.Module SrcSpanInfo
--type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
type Decl = A.Decl SrcSpanInfo

class Spans a where
    spans :: a -> [SrcSpanInfo]

instance Spans (A.Module SrcSpanInfo) where
    spans (A.Module _sp mh ps is ds) =
        concatMap spans ps ++ maybe [] spans mh ++ concatMap spans is ++ concatMap spans ds
    spans _ = error "spans XML module"

instance Spans (A.ModuleHead SrcSpanInfo) where
    spans (A.ModuleHead _ n mw me) = spans n ++ maybe [] spans mw ++ maybe [] spans me

instance Spans (A.ExportSpecList SrcSpanInfo) where
    spans (A.ExportSpecList _ es) = concatMap spans es

instance Spans (A.ExportSpec SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.ModulePragma SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.ImportDecl SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.Decl SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.ModuleName SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.WarningText SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]

-- This happens, a span with end column 0, even though column
-- numbering begins at 1.  Is it a bug in haskell-src-exts?
fixSpan :: SrcSpanInfo -> SrcSpanInfo
fixSpan sp =
    if srcSpanEndColumn (srcInfoSpan sp) == 0
    then sp {srcInfoSpan = (srcInfoSpan sp) {srcSpanEndColumn = 1}}
    else sp

data St
    = St { loc_ :: SrcLoc
         , text_ :: String
         , comms_ :: [Comment]
         , sps_ :: [SrcSpanInfo] }
      deriving (Show)

setSpanEnd :: SrcLoc -> SrcSpan -> SrcSpan
setSpanEnd loc sp = sp {srcSpanEndLine = srcLine loc, srcSpanEndColumn = srcColumn loc}
-- setSpanStart :: SrcLoc -> SrcSpan -> SrcSpan
-- setSpanStart loc sp = sp {srcSpanStartLine = srcLine loc, srcSpanStartColumn = srcColumn loc}

-- | The spans returned by haskell-src-exts may put comments and
-- whitespace in the suffix string of a declaration, we want them in
-- the prefix string of the following declaration where possible.
adjustSpans :: String -> [Comment] -> [SrcSpanInfo] -> [SrcSpanInfo]
adjustSpans _ _ [] = []
adjustSpans _ _ [x] = [x]
adjustSpans text comments sps@(x : _) =
    fst $ runState f (St (SrcLoc (srcFilename (srcLoc x)) 1 1) text comments sps)
    where
      f = do st <- get
             let b = loc_ st
             case sps_ st of
               (ss1 : ssis) ->
                   do skip
                      st' <- get
                      let e = loc_ st'
                      case e >= endLoc ss1 of
                        True ->
                            -- We reached the end of ss1, so the segment from b to e is
                            -- trailing comments and space, some of which may belong in
                            -- the following span.
                            do put (st' {sps_ = ssis})
                               sps' <- f
                               let ss1' = ss1 {srcInfoSpan = setSpanEnd b (srcInfoSpan ss1)}
                               return (ss1' : sps')
                        False ->
                           -- If we weren't able to skip to the end of
                           -- the span, we encountered real text.
                           -- Move past one char and try again.
                           do case text_ st' of
                                "" -> return (sps_ st') -- error $ "Ran out of text\n st=" ++ show st ++ "\n st'=" ++ show st'
                                (c : t') -> do put (st' {text_ = t', loc_ = increaseSrcLoc [c] e})
                                               f
               sss -> return sss

      -- loc is the current position in the input file, text
      -- is the text starting at that location.
      skip :: State St ()
      skip = do loc1 <- loc_ <$> get
                skipWhite
                skipComment
                loc2 <- loc_ <$> get
                -- Repeat until failure
                when (loc1 /= loc2) skip

      skipWhite :: State St ()
      skipWhite = do st <- get
                     case span isSpace (text_ st) of
                       ("", _) -> return ()
                       (space, t') ->
                           let loc' = increaseSrcLoc space (loc_ st) in
                           put (st {loc_ = loc', text_ = t'})

      skipComment :: State St ()
      skipComment = do st <- get
                       case comms_ st of
                         (Comment _ csp _ : cs)
                             | srcLoc csp <= loc_ st ->
                                 -- We reached the comment, skip past it and discard
                                 case srcPairText (loc_ st) (endLoc csp) (text_ st) of
                                   ("", _) -> return ()
                                   (comm, t') ->
                                       let loc' = increaseSrcLoc comm (loc_ st) in
                                       put (st {loc_ = loc',
                                                text_ = t',
                                                comms_ = cs})
                         _ -> return () -- No comments, or we didn't reach it

-- | Given the result of parseModuleWithComments and the original
-- module text, this does a fold over the parsed module contents,
-- calling the seven argument functions in order.  Each function is
-- passed the AST value, the text of the space and comments leading up
-- to the element, and the text for the element.  Note that not
-- everything passed to the "pre" argument of the functions will be
-- comments and space - for example, the "module" keyword will be
-- passed in the pre argument to the ModuleName function.
foldModuleM :: forall m r. (Monad m, Functor m, Show r) =>
              (String -> r -> m r) -- ^ Receives the space before the first pragma.
           -> (ModulePragma -> String -> String -> String -> r -> m r) -- ^ Called once for each pragma.  In this and the similar arguments below, the three string arguments contain
                                                                     -- the comments and space preceding the construct, the text of the construct and the space following it.
           -> (ModuleName -> String -> String -> String -> r -> m r) -- ^ Called with the module name.
           -> (WarningText -> String -> String -> String -> r -> m r) -- ^ Called with the warning text between module name and export list
           -> (String -> r -> m r) -- ^ Called with the export list open paren
           -> (ExportSpec -> String -> String -> String -> r -> m r) -- ^ Called with each export specifier
           -> (String -> r -> m r) -- ^ Called with the export list close paren and "where" keyword
           -> (ImportDecl -> String -> String -> String -> r -> m r) -- ^ Called with each import declarator
           -> (Decl -> String -> String -> String -> r -> m r) -- ^ Called with each top level declaration
           -> (String -> r -> m r) -- ^ Called with comments following the last declaration
           -> ModuleInfo -- ^ Parsed module
           -> r -- ^ Fold initialization value
           -> m r -- ^ Result
foldModuleM _ _ _ _ _ _ _ _ _ _ (ModuleInfo {_module = A.XmlPage _ _ _ _ _ _ _}) _ = error "XmlPage: unsupported"
foldModuleM _ _ _ _ _ _ _ _ _ _ (ModuleInfo {_module = A.XmlHybrid _ _ _ _ _ _ _ _ _}) _ = error "XmlHybrid: unsupported"
foldModuleM topf pragmaf namef warnf pref exportf postf importf declf sepf (ModuleInfo {_module = m@(A.Module (SrcSpanInfo (SrcSpan path _ _ _ _) _) mh ps is ds), _moduleText = text, _moduleComments = comments}) r0 =
    (\ (_, (_, _, _, r)) -> r) <$> runStateT doModule (text, (SrcLoc path 1 1), spans m, r0)
    where
      doModule :: Monad m => StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doModule =
          do doSep topf
             doList pragmaf ps
             maybe (pure ()) doHeader mh
             (tl, l, sps, r) <- get
             put (tl, l, adjustSpans text comments sps, r)
             doList importf is
             doList declf ds
             doTail sepf
      doHeader :: Monad m => A.ModuleHead SrcSpanInfo -> StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doHeader (A.ModuleHead sp n mw me) =
          do doItem namef n
             maybe (pure ()) (doItem warnf) mw
             doSep pref
             maybe (pure ()) (\ (A.ExportSpecList _ es) -> doList exportf es) me
             doClose postf sp
      doClose :: Monad m => (String -> r -> m r) -> SrcSpanInfo -> StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doClose f sp =
          do (tl, l, sps, r) <- get
             case l < endLoc sp of
               True -> do
                 let (p, s) = srcPairText l (endLoc sp) tl
                 r' <- lift $ f p r
                 put (s, endLoc sp, sps, r')
               False -> pure ()
      doTail :: Monad m => (String -> r -> m r) -> StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doTail f =
          do (tl, l, sps, r) <- get
             r' <- lift $ f tl r
             put (tl, l, sps, r')
      doSep :: Monad m => (String -> r -> m r) -> StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doSep f =
          do p <- get
             case p of
               (tl, l, sps@(sp : _), r) ->
                   do let l' = srcLoc sp
                      case l <= l' of
                        True -> do
                          let (b, a) = srcPairText l l' tl
                          r' <- lift $ f b r
                          put (a, l', sps, r')
                        False -> pure ()
               _ -> error $ "foldModule - out of spans: " ++ show p
      doList :: (HasSpanInfo a, Show a) => (a -> String -> String -> String -> r -> m r) -> [a] -> StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doList _ [] = pure ()
      doList f (x : xs) = doItem f x >> doList f xs

      doItem :: (HasSpanInfo a, Show a) => (a -> String -> String -> String -> r -> m r) -> a -> StateT (String, SrcLoc, [SrcSpanInfo], r) m ()
      doItem f x =
          do (tl, l, (sp : sps'), r) <- get
             let -- Another haskell-src-exts bug?  If a module ends
                 -- with no newline, endLoc will be at the beginning
                 -- of the following (nonexistant) line.
                 (pre, tl') = srcPairText l (srcLoc sp) tl
                 l' = endLoc sp
                 (s, tl'') = srcPairText (srcLoc sp) l' tl'
                 l'' = adjust1 tl'' l'
                 (post, tl''') = srcPairText l' l'' tl''
             r' <- lift $ f x pre s post r
             put (tl''', l'', sps', r')

      -- Move to just past the last newline in the leading whitespace
      -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) ->
      --   (SrcLoc "<unknown>.hs" 7 1)
      _adjust :: String -> SrcLoc -> SrcLoc
      _adjust a l =
          l'
          where
            w = takeWhile isSpace a
            w' = take (length (takeWhile (elem '\n') (tails w))) w
            l' = increaseSrcLoc w' l

      -- Move to just past the first newline in the leading whitespace
      -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) ->
      --   (SrcLoc "<unknown>.hs" 6 1)
      adjust1 :: String -> SrcLoc -> SrcLoc
      adjust1 a l =
          l'
          where
            w = takeWhile isSpace a
            w' = case span (/= '\n') w of
                   (w'', '\n' : _) -> w'' ++ ['\n']
                   (w'', "") -> w''
                   _ -> error "Impossible: span stopped on the wrong char"
            l' = increaseSrcLoc w' l

-- | Do just the header portion of 'foldModule'.
foldHeaderM :: forall m r. (Monad m, Show r) =>
              (String -> r -> m r)
           -> (ModulePragma -> String -> String -> String -> r -> m r)
           -> (ModuleName -> String -> String -> String -> r -> m r)
           -> (WarningText -> String -> String -> String -> r -> m r)
           -> ModuleInfo -> r -> m r
foldHeaderM topf pragmaf namef warnf m r0 =
    foldModuleM topf pragmaf namef warnf ignore2M ignoreM ignore2M ignoreM ignoreM ignore2M m r0

-- | Do just the exports portion of 'foldModule'.
foldExportsM :: forall m r. (Monad m, Show r) =>
               (String -> r -> m r)
            -> (ExportSpec -> String -> String -> String -> r -> m r)
            -> (String -> r -> m r)
            -> ModuleInfo -> r -> m r
foldExportsM pref exportf postf m r0 =
    foldModuleM ignore2M ignoreM ignoreM ignoreM pref exportf postf ignoreM ignoreM ignore2M m r0

-- | Do just the imports portion of 'foldModule'.
foldImportsM :: forall m r. (Monad m, Show r) =>
               (ImportDecl -> String -> String -> String -> r -> m r)
            -> ModuleInfo -> r -> m r
foldImportsM importf m r0 =
    foldModuleM ignore2M ignoreM ignoreM ignoreM ignore2M ignoreM ignore2M importf ignoreM ignore2M m r0

-- | Do just the declarations portion of 'foldModule'.
foldDeclsM :: forall m r. (Monad m, Show r) =>
             (Decl -> String -> String -> String -> r -> m r)
          -> (String -> r -> m r)
          -> ModuleInfo -> r -> m r
foldDeclsM declf sepf m r0 =
    foldModuleM ignore2M ignoreM ignoreM ignoreM ignore2M ignoreM ignore2M ignoreM declf sepf m r0

-- | This can be passed to foldModule to include the original text in the result
echoM :: (Monoid s, Monad m) => t -> s -> s -> s -> Seq s -> m (Seq s)
echoM _ pref s suff r = pure $ r |> pref <> s <> suff

-- | Similar to 'echo', but used for the two argument separator functions
echo2M :: (Monoid s, Monad m) => s -> Seq s -> m (Seq s)
echo2M s r = pure $ r |> s

-- | This can be passed to foldModule to omit the original text from the result.
ignoreM :: forall t s m r. Monad m => t -> s -> s -> s -> r -> m r
ignoreM _ _ _ _ r = pure r

-- | Similar to 'ignore', but used for the two argument separator functions
ignore2M :: forall s m r. Monad m => s -> r -> m r
ignore2M _ r = pure r
