{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Types
    ( ModuleInfo(..)
    , fullPathOfModuleInfo
    , hseExtensions
    , hsFlags
    , hsSourceDirs
    , loadModule
    , loadModule'
    , loadModules
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
import SrcLoc (fixSpan, textSpan)
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

  -- | From hsx2hs, but removing Arrows because it makes test case
-- fold3c and others fail.  Maybe we should parse the headers and then
-- use the options there?  There are functions to do this.
hseExtensions :: [Extension]
hseExtensions = map nameToExtension
    [ RecursiveDo, ParallelListComp, MultiParamTypeClasses, FunctionalDependencies, RankNTypes, ExistentialQuantification
    , ScopedTypeVariables, ImplicitParams, FlexibleContexts, FlexibleInstances, EmptyDataDecls, KindSignatures
    , BangPatterns, TemplateHaskell, ForeignFunctionInterface, {- Arrows, -} DeriveGeneric, NamedFieldPuns, PatternGuards
    , MagicHash, TypeFamilies, StandaloneDeriving, TypeOperators, RecordWildCards, GADTs, UnboxedTuples
    , PackageImports, QuasiQuotes, {-TransformListComp,-} ViewPatterns, {-XmlSyntax, RegularPatterns,-} TupleSections
    , ExplicitNamespaces
    ]
    where
      nameToExtension :: KnownExtension -> Extension
      nameToExtension x = EnableExtension x

hsFlags :: [String]
hsFlags = []

hsSourceDirs :: [FilePath]
hsSourceDirs = []

loadModules :: [FilePath] -> IO [ModuleInfo]
loadModules paths = t1 <$> mapM loadModule' paths
    where
      t1 :: [ModuleInfo] -> [ModuleInfo]
      t1 modules = trace ("modules loaded: " ++ show (map ezPrint modules)) modules

loadModule' :: FilePath -> IO ModuleInfo
loadModule' path = either (error . show) id <$> (loadModule path :: IO (Either SomeException ModuleInfo))

loadModule :: Exception e => FilePath -> IO (Either e ModuleInfo)
loadModule path =
  try loadModule' {- `IO.catch` (\(e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e) -}
    where
      loadModule' :: IO ModuleInfo
      loadModule' = do
        moduleText <- liftIO $ readFile path
        (parsed', comments, processed) <- Exts.fromParseResult <$> CPP.parseFileWithCommentsAndCPP cpphsOptions mode path
        let parsed = everywhere (mkT fixSpan) parsed'
        -- liftIO $ writeFile (path ++ ".cpp") processed
        -- putStr processed
        -- validateParseResults parsed comments processed -- moduleText
        key <- moduleKey path parsed
        putStrLn ("loaded " ++ prettyShow key)
        pure $ ModuleInfo { _moduleKey = key
                          , _module = parsed
                          , _moduleComments = comments
                          , _modulePath = makeRelative (case key of
                                                          ModuleKey {_moduleTop = top} -> top
                                                          ModuleFullPath p -> takeDirectory p) path
                          , _moduleText = moduleText
                          , _moduleSpan = textSpan path moduleText }
      mode = Exts.defaultParseMode {Exts.extensions = hseExtensions, Exts.parseFilename = path, Exts.fixities = Nothing }

-- | Turn of the locations flag.  This means simple #if macros will not
-- affect the line numbers of the output text, so we can use the
-- resulting SrcSpan info on the original text.  Macro expansions
-- could still mess this up.
cpphsOptions :: CPP.CpphsOptions
cpphsOptions =
    CPP.defaultCpphsOptions
    { CPP.boolopts =
          (CPP.boolopts CPP.defaultCpphsOptions)
          { CPP.locations = False
      }
    }

-- | Compute the module key from a filepath (absolute or relative to
-- ".") and the parsed module.
moduleKey :: FilePath -> A.Module SrcSpanInfo -> IO ModuleKey
moduleKey _ (A.XmlPage {}) = error "XmlPage"
moduleKey _ (A.XmlHybrid {}) = error "XmlHybrid"
moduleKey path (A.Module _ Nothing _ _ _) = do
  ModuleFullPath <$> canonicalizePath path
moduleKey path (A.Module _ (Just (A.ModuleHead _ (A.ModuleName _ name) _ _)) _ _ _) = do
  path' <- canonicalizePath path
  let name' = splitModuleName name
      (path'', ext) = splitExtension path'
      dirs = splitDirectories path''
      (dirs', name'') = splitAt (length dirs - length name') dirs
  when (name'' /= name') (error $ "Module name mismatch - name: " ++ show name' ++ ", path: " ++ show name'')
  pure $ ModuleKey (joinPath dirs')
                   (S.ModuleName (intercalate "." name''))
                   ext
      where
        splitModuleName = filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))
