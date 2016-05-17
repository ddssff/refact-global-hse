{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Fold
    ( foldModule
    , foldHeader
    , foldExports
    , foldImports
    , foldDecls
    , echo
    , echo2
    , ignore
    , ignore2
    ) where

import Control.Lens ((%=), (.=), (.~), makeLenses, use, view)
import Control.Monad (when)
import Control.Monad.State (get, runState, State)
import Data.Char (isSpace)
import Data.List (tails)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>))
--import Debug.Trace (trace)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Annotated(ann), Decl, ExportSpec, ExportSpec(..), ExportSpecList(ExportSpecList), ImportDecl, Module(..), ModuleHead(..), ModuleName, ModulePragma, WarningText)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..), srcSpanEndColumn)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
-- import Language.Haskell.Modules.ModuVerse (ModuleInfo(ModuleInfo))
import qualified Text.PrettyPrint as Pretty (text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint){-, prettyShow-})
import Prelude hiding (tail)
import SrcLoc (endLoc, increaseSrcLoc, srcLoc, srcPairText, srcSpan)
import Types (ModuleInfo(ModuleInfo, _module, _moduleComments, _moduleText))

{-
data SrcSpanInfo = SrcSpanInfo
    { srcInfoSpan    :: SrcSpan
    , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
    }
  deriving (Eq,Ord,Typeable,Data)

data SrcSpan = SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Eq,Ord,Typeable,Data)

instance Pretty SrcLoc where
    pPrint SrcLoc{..} = Pretty.text ("(l" ++ show srcLine ++ ",c" ++ show srcColumn ++ ")")

instance Pretty SrcSpanInfo where
    pPrint (SrcSpanInfo {srcInfoSpan = SrcSpan {..}}) =
        Pretty.text ("[(l" ++ show srcSpanStartLine ++ ",c" ++ show srcSpanStartColumn ++ ")->" ++
                      "(l" ++ show srcSpanEndLine ++ ",c" ++ show srcSpanEndColumn ++ ")]")
-}

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

instance Spans (A.ExportSpec SrcSpanInfo) where spans x = [A.ann x]
instance Spans (A.ModulePragma SrcSpanInfo) where spans x = [A.ann x]
instance Spans (A.ImportDecl SrcSpanInfo) where spans x = [A.ann x]
instance Spans (A.Decl SrcSpanInfo) where spans x = [A.ann x]
instance Spans (A.ModuleName SrcSpanInfo) where spans x = [A.ann x]
instance Spans (A.WarningText SrcSpanInfo) where spans x = [A.ann x]

data St
    = St { _loc :: SrcLoc
         , _text :: String
         , _comms :: [Comment]
         , _sps :: [SrcSpanInfo] }
      deriving (Show)

$(makeLenses ''St)

srcLoc' :: SrcSpanInfo -> SrcLoc
srcLoc' = srcLoc'' . srcInfoSpan

endLoc' :: SrcSpanInfo -> SrcLoc
endLoc' = endLoc'' . srcInfoSpan

srcLoc'' :: SrcSpan -> SrcLoc
srcLoc'' (SrcSpan f b e _ _) = SrcLoc f b e

endLoc'' :: SrcSpan -> SrcLoc
endLoc'' (SrcSpan f _ _ b e) = SrcLoc f b e

setSpanEnd :: SrcLoc -> SrcSpan -> SrcSpan
setSpanEnd loc' sp = sp {srcSpanEndLine = srcLine loc', srcSpanEndColumn = srcColumn loc'}
-- setSpanStart :: SrcLoc -> SrcSpan -> SrcSpan
-- setSpanStart loc sp = sp {srcSpanStartLine = srcLine loc, srcSpanStartColumn = srcColumn loc}

-- | The spans returned by haskell-src-exts may put comments and
-- whitespace in the suffix string of a declaration, we want them in
-- the prefix string of the following declaration where possible.  (I
-- think the behavior that made this necessary may have been fixed in
-- haskell-src-exts.)
adjustSpans :: String -> [Comment] -> [SrcSpanInfo] -> [SrcSpanInfo]
adjustSpans _ _ [] = []
adjustSpans _ _ [x] = [x]
adjustSpans text0 comments sps0@(x : _) =
    fst $ runState f (St (SrcLoc (srcFilename (srcLoc' x)) 1 1) text0 comments sps0)
    where
      f = do b <- use loc
             sss <- use sps
             case sss of
               (ss1 : ssis) ->
                   do skip
                      e <- use loc
                      let e' = endLoc' ss1
                      case e >= e' of
                        True ->
                            -- We reached the end of ss1, so the segment from b to e is
                            -- trailing comments and space, some of which may belong in
                            -- the following span.
                            do -- t <- use text
                               -- let (p, s) = srcPairText e' e t
                               -- trace ("p=" ++ show p ++ ", s=" ++ show s) (pure ())
                               sps .= ssis
                               sps' <- f
                               let ss1' = ss1 {srcInfoSpan = setSpanEnd b (srcInfoSpan ss1)}
                               when (ss1' /= ss1) ({-trace ("span adjusted: " ++ prettyShow ss1 ++ " -> " ++ prettyShow ss1')-} (return ()))
                               return (ss1' : sps')
                        False ->
                           -- If we weren't able to skip to the end of
                           -- the span, we encountered real text.
                           -- Move past one char and try again.
                           do t <- use text
                              case t of
                                "" -> use sps -- error $ "Ran out of text\n st=" ++ show st ++ "\n st'=" ++ show st'
                                (c : t') -> do text .= t'
                                               loc .= increaseSrcLoc [c] e
                                               f
               [] -> return []

      -- loc is the current position in the input file, text
      -- is the text starting at that location.
      skip :: State St ()
      skip = do loc1 <- _loc <$> get
                skipWhite
                skipComment
                loc2 <- _loc <$> get
                -- Repeat until failure
                when (loc1 /= loc2) skip

      skipWhite :: State St ()
      skipWhite = do t <- use text
                     case span isSpace t of
                       ("", _) -> return ()
                       (space, t') -> do
                         loc %= increaseSrcLoc space
                         text .= t'

      skipComment :: State St ()
      skipComment = do cs' <- use comms
                       l <- use loc
                       t <- use text
                       case cs' of
                         (Comment _ csp _ : cs)
                             | srcLoc'' csp <= l ->
                                 -- We reached the comment, skip past it and discard
                                 case srcPairText l (endLoc'' csp) t of
                                   ("", _) -> return ()
                                   (comm, t') -> do
                                     loc %= increaseSrcLoc comm
                                     text .= t'
                                     comms .= cs
                         _ -> return () -- No comments, or we didn't reach it

{-
deriving instance Ord Comment

data ModuleInfo
    = ModuleInfo
      { module_ :: A.Module SrcSpanInfo
      , modtext_ :: String
      , comments_ :: [Comment]
      , key_ :: ModKey
      , name_ :: S.ModuleName }
    deriving (Eq, Ord, Show)
-}

data St2 r
    = St2 { _tail :: String
          , _srcloc :: SrcLoc
          , _srcspans :: [SrcSpanInfo]
          , _result :: r }

$(makeLenses ''St2)

-- | Given the result of parseModuleWithComments and the original
-- module text, this does a fold over the parsed module contents,
-- calling the seven argument functions in order.  Each function is
-- passed the AST value, the text of the space and comments leading up
-- to the element, and the text for the element.  Note that not
-- everything passed to the "pre" argument of the functions will be
-- comments and space - for example, the "module" keyword will be
-- passed in the pre argument to the ModuleName function.
foldModule :: forall r. (Show r) =>
              (String -> r -> r) -- ^ Receives the space before the first pragma.
           -> (ModulePragma -> String -> String -> String -> r -> r) -- ^ Called once for each pragma.  In this and the similar arguments below, the three string arguments contain
                                                                     -- the comments and space preceding the construct, the text of the construct and the space following it.
           -> (ModuleName -> String -> String -> String -> r -> r) -- ^ Called with the module name.
           -> (WarningText -> String -> String -> String -> r -> r) -- ^ Called with the warning text between module name and export list
           -> (String -> r -> r) -- ^ Called with the export list open paren
           -> (ExportSpec -> String -> String -> String -> r -> r) -- ^ Called with each export specifier
           -> (String -> r -> r) -- ^ Called with the export list close paren and "where" keyword
           -> (ImportDecl -> String -> String -> String -> r -> r) -- ^ Called with each import declarator
           -> (Decl -> String -> String -> String -> r -> r) -- ^ Called with each top level declaration
           -> (String -> r -> r) -- ^ Called with comments following the last declaration
           -> ModuleInfo -- ^ Parsed module
           -> r -- ^ Fold initialization value
           -> r -- ^ Result
foldModule _ _ _ _ _ _ _ _ _ _ (ModuleInfo {_module = A.XmlPage _ _ _ _ _ _ _}) _ = error "XmlPage: unsupported"
foldModule _ _ _ _ _ _ _ _ _ _ (ModuleInfo {_module = A.XmlHybrid _ _ _ _ _ _ _ _ _}) _ = error "XmlHybrid: unsupported"
foldModule topf pragmaf namef warnf pref exportf postf importf declf sepf (ModuleInfo { _module = m@(A.Module (SrcSpanInfo (SrcSpan path _ _ _ _) _) mh ps is ds)
                                                                                      , _moduleText = moduletext
                                                                                      , _moduleComments = comments }) r0 =
    (\ (_, st) -> view result st) $ runState doModule (St2 moduletext (SrcLoc path 1 1) ({-t2-} (spans m)) r0)
    where
      -- t2 spans = trace ("module spans: " ++ prettyShow spans) spans
      doModule :: State (St2 r) ()
      doModule =
          do doSep topf
             doList pragmaf ps
             maybe (pure ()) doHeader mh
             srcspans %= adjustSpans moduletext comments
             doList importf is
             doList declf ds
             doTail sepf
      doHeader :: A.ModuleHead SrcSpanInfo -> State (St2 r) ()
      doHeader (A.ModuleHead sp n mw me) =
          do doItem namef n
             maybe (return ()) (doItem warnf) mw
             doSep pref
             maybe (return ()) (\ (A.ExportSpecList _ es) -> doList exportf es) me
             doClose postf sp
      doClose :: (String -> r -> r) -> SrcSpanInfo -> State (St2 r) ()
      doClose f sp =
          do tl <- use tail
             l <- use srcloc
             case l < endLoc' sp of
               True -> do
                 let (p, s) = srcPairText l (endLoc' sp) tl
                 tail .= s
                 srcloc .= endLoc' sp
                 result %= f p
               False -> return ()
      doTail :: (String -> r -> r) -> State (St2 r) ()
      doTail f =
          do tl <- use tail
             result %= f tl
      doSep :: (String -> r -> r) -> State (St2 r) ()
      doSep f =
          do tl <- use tail
             l <- use srcloc
             spans' <- use srcspans
             case spans' of
               (sp : _) ->
                   do let l' = srcLoc' sp
                      case l <= l' of
                        True -> do
                          let (b, a) = srcPairText l l' tl
                          tail .= a
                          srcloc .= l'
                          result %= f b
                        False -> return ()
               _ -> error $ "foldModule - out of spans"
      doList :: (A.Annotated a, Show (a SrcSpanInfo)) => (a SrcSpanInfo -> String -> String -> String -> r -> r) -> [a SrcSpanInfo] -> State (St2 r) ()
      doList _ [] = return ()
      doList f (x : xs) = doItem f x >> doList f xs

      doItem :: (A.Annotated a, Show (a SrcSpanInfo)) => (a SrcSpanInfo -> String -> String -> String -> r -> r) -> a SrcSpanInfo -> State (St2 r) ()
      doItem f x =
          do tl <- use tail
             l <- use srcloc
             spans' <- use srcspans
             case spans' of
               (sp : sps') -> do
                 let -- Another haskell-src-exts bug?  If a module ends
                     -- with no newline, endLoc will be at the beginning
                     -- of the following (nonexistant) line.
                     (pre, tl') = srcPairText l (srcLoc' sp) tl
                     l' = endLoc' sp
                     (s, tl'') = srcPairText (srcLoc' sp) l' tl'
                     l'' = adjust1 tl'' l'
                     (post, tl''') = srcPairText l' l'' tl''
                 tail .= tl'''
                 srcloc .= l''
                 srcspans .= sps'
                 result %= f x pre s post

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
foldHeader :: forall r. (Show r) =>
              (String -> r -> r)
           -> (ModulePragma -> String -> String -> String -> r -> r)
           -> (ModuleName -> String -> String -> String -> r -> r)
           -> (WarningText -> String -> String -> String -> r -> r)
           -> ModuleInfo -> r -> r
foldHeader topf pragmaf namef warnf m r0 =
    foldModule topf pragmaf namef warnf ignore2 ignore ignore2 ignore ignore ignore2 m r0

-- | Do just the exports portion of 'foldModule'.
foldExports :: forall r. (Show r) =>
               (String -> r -> r)
            -> (ExportSpec -> String -> String -> String -> r -> r)
            -> (String -> r -> r)
            -> ModuleInfo -> r -> r
foldExports pref exportf postf m r0 =
    foldModule ignore2 ignore ignore ignore pref exportf postf ignore ignore ignore2 m r0

-- | Do just the imports portion of 'foldModule'.
foldImports :: forall r. (Show r) =>
               (ImportDecl -> String -> String -> String -> r -> r)
            -> ModuleInfo -> r -> r
foldImports importf m r0 =
    foldModule ignore2 ignore ignore ignore ignore2 ignore ignore2 importf ignore ignore2 m r0

-- | Do just the declarations portion of 'foldModule'.
foldDecls :: forall r. (Show r) =>
             (Decl -> String -> String -> String -> r -> r)
          -> (String -> r -> r)
          -> ModuleInfo -> r -> r
foldDecls declf sepf m r0 =
    foldModule ignore2 ignore ignore ignore ignore2 ignore ignore2 ignore declf sepf m r0

-- | This can be passed to foldModule to include the original text in the result
echo :: Monoid m => t -> m -> m -> m -> Seq m -> Seq m
echo _ pref s suff r = r |> pref <> s <> suff

-- | Similar to 'echo', but used for the two argument separator functions
echo2 :: Monoid m => m -> Seq m -> Seq m
echo2 s r = r |> s

-- | This can be passed to foldModule to omit the original text from the result.
ignore :: t -> m -> m -> m -> r -> r
ignore _ _ _ _ r = r

-- | Similar to 'ignore', but used for the two argument separator functions
ignore2 :: m -> r -> r
ignore2 _ r = r
