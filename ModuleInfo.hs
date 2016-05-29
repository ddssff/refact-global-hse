{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module ModuleInfo
    ( ModuleInfo(..)
    , fullPathOfModuleInfo
    ) where

import qualified CPP (BoolOptions(locations), CpphsOptions(boolopts), defaultCpphsOptions, parseFileWithCommentsAndCPP)
import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted as IO (try)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate)
import Debug.Trace (trace)
import qualified Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions, parseFilename, fixities), fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import ModuleKey (ModuleKey(ModuleFullPath, ModuleKey, _moduleTop), moduleFullPath)
import System.Directory (canonicalizePath)
import System.FilePath (joinPath, makeRelative, splitDirectories, splitExtension, takeDirectory)
import Text.PrettyPrint.HughesPJClass as PP (prettyShow)
import Utils (EZPrint(ezPrint))

data ModuleInfo =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: A.Module SrcSpanInfo
               , _moduleComments :: [Comment]
               , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               }

instance EZPrint ModuleInfo where
    ezPrint (ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ n _ _)) _ _ _}) = prettyPrint n
    ezPrint (ModuleInfo {_module = A.Module _ Nothing _ _ _}) = "Main"
    ezPrint (ModuleInfo {_module = _}) = error "ezPrint: unexpected module"

fullPathOfModuleInfo :: ModuleInfo -> FilePath
fullPathOfModuleInfo = moduleFullPath . _moduleKey
