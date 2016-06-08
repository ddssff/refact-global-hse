{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Scan(St(St, _point, _remaining, _comments)
    , SpanInfo(srcSpan)
    , srcLoc
    , locDiff
    , spanDiff
    , locSum
    , endLocOfText
    , textOfSpan
    , testSpan
    , splitText
    , ScanM()
    , keep
    , skip
    , trailingWhitespace
    , withTrailingWhitespace
    , debugRender
    , void
    , EndLoc(endLoc)
    ) where

import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Names
import ModuleInfo
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint), lines')
import Control.Lens ((.=), (%=), makeLenses, makeLensesFor, use, view)
import Control.Monad.RWS (evalRWS, MonadWriter(tell), RWS)
import Control.Monad.State (get, put, runState, State)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcInfo(..), SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
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

instance A.Annotated ast => SpanInfo (ast SrcSpanInfo) where
    srcSpan = srcSpan . A.ann

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

textOfSpan :: (SrcInfo a, EndLoc a) => a -> String -> String
textOfSpan sp s =
    let (_, s') = splitText (srcLoc sp) s in
    let (s'', _) = splitText (locDiff (endLoc sp) (srcLoc sp)) s' in
    s''

testSpan :: (SrcInfo a, EndLoc a) => String -> a -> a
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
                     _ -> error "splitText"
               (_, LT) ->
                   case s of
                     [] -> error ("splitText " ++ ", loc=" ++ show loc ++ ", s=" ++ show s)
                     (ch : s') -> put (l, c + 1, r ++ [ch], s') >> f
               (EQ, EQ) -> pure (r, s)
               _ -> error ("splitText - invalid arguments: loc=" ++ show loc ++ ", s=" ++ show s0)

type ScanM = RWS () String St

keep :: SrcLoc -> ScanM ()
keep loc = do
  t' <- use remaining
  p <- use point
  let (s', t'') = splitText (locDiff (max p loc) p) t'
  tell s'
  remaining .= t''
  point .= {-trace ("keep " ++ show loc)-} loc
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)

skip :: SrcLoc -> ScanM ()
skip loc = do
  p <- use point
  pure $ testSpan "skip" (SrcSpan (srcFilename loc) (srcLine p) (srcColumn p) (srcLine loc) (srcColumn loc))
  t' <- use remaining
  let (_, t'') = splitText (locDiff (max p loc) p) t'
  remaining .= t''
  point .= {-trace ("skip " ++ show loc)-} loc
  comments %= dropWhile (\(Comment _ sp _) -> loc > srcLoc sp)

-- | Assuming the spans of the ast have been adjusted (tightened)
-- using fixEnds, look at the text between point and the beginning of
-- the next span and decide which part belongs to the preceding
-- declaration (or import or whatever) and which belongs to the next
-- one.
trailingWhitespace :: Maybe SrcLoc -> ScanM String
trailingWhitespace next = do
  t <- use remaining
  loc@(SrcLoc file _ _) <- use point
  let loc'' = maybe (locSum loc (endLocOfText file t)) id next
  case loc'' >= loc of
    False -> error $ "trailingWhitespace: " ++ show loc'' ++ " < " ++ show loc
    True -> do
      let (s', _) = splitText (locDiff loc'' loc) t
      case lines' s' of
        [] -> pure s'
        (x : xs) ->
            -- x is the end of the last line of the declaration (or
            -- whatever), so we always keep that.  Also keep subsequent
            -- nonblank lines.
            case break (all isSpace) xs of
              (_, []) -> pure s'
              (comments', _) -> pure (unlines (x : comments'))

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
debugRender _ _ _ = error "debugRender"

void :: Monad m => m ()
void = pure ()

class EndLoc a where endLoc :: a -> SrcLoc
instance EndLoc SrcSpan where endLoc x = SrcLoc (fileName x) (srcSpanEndLine x) (srcSpanEndColumn x)
instance EndLoc SrcSpanInfo where endLoc = endLoc . srcInfoSpan
instance EndLoc a => EndLoc (Scoped a) where endLoc (Scoped _ x) = endLoc x
instance EndLoc (SrcLoc, SrcLoc) where endLoc = snd
