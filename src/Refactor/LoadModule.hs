{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}

module Refactor.LoadModule
    ( loadModule
    , loadModules
    ) where

import Control.Lens (over, view)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate, nub)
import Debug.Trace (trace)
import Language.Haskell.Exts.CPP (parseFileWithCommentsAndCPP)
import Language.Haskell.Exts.Syntax (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Parser as Exts (fromParseResult, ParseMode(extensions, parseFilename, fixities))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Names (annotate, resolve, Scoped(..))
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Preprocessor.Cpphs (BoolOptions(locations), CpphsOptions(..))
import Refactor.CPP (applyHashDefine, applyHashDefine', cppOptions, defaultParseMode, enabled, extensionsForHSEParser,
                     GHCOpts, hashDefines, turnOffLocations)
import qualified Refactor.CPP (defaultCpphsOptions)
import Refactor.ModuleInfo (ModuleInfo(..))
import Refactor.ModuleKey (ModuleKey(..))
import Refactor.SrcLoc (fixEnds, fixSpan, mapTopAnnotations, spanOfText)
import Refactor.Utils (EZPrint(ezPrint))
import System.Directory (canonicalizePath)
import System.FilePath ((</>), joinPath, makeRelative, splitDirectories, splitExtension, takeDirectory)
import System.IO (hPutStrLn, stderr)

instance EZPrint (Scoped SrcSpanInfo) where
    ezPrint (Scoped _ x) = ezPrint x

-- | Load a list of modules and compute their global scoping info.  As
-- of right now the scoping information is not actually used, but
-- surely there will be some use for it down the road.
loadModules :: GHCOpts -> [(Maybe FilePath, FilePath)] -> IO [ModuleInfo (Scoped SrcSpanInfo)]
loadModules opts pairs = do
  t1 <$> addScoping <$> mapM (loadModule opts) (nub pairs)
    where
      t1 :: [ModuleInfo l] -> [ModuleInfo l]
      t1 modules = trace ("modules loaded: " ++ show (map ezPrint modules)) modules

addScoping :: [ModuleInfo SrcSpanInfo] -> [ModuleInfo (Scoped SrcSpanInfo)]
addScoping mods =
    map (\m -> m {_module = annotate env (_module m),
                  _moduleGlobals = moduleTable (importTable env (_module m)) (_module m)}) mods
    where env = resolve (map _module mods) mempty

pairPath :: (Maybe FilePath, FilePath) -> FilePath
pairPath (mtop, path) = maybe path (\top -> top </> path) mtop

-- | Load a single module.
loadModule :: GHCOpts -> (Maybe FilePath, FilePath) -> IO (ModuleInfo SrcSpanInfo)
loadModule opts pair = do
  let path = pairPath pair
      mode = defaultParseMode opts path
  moduleText <- liftIO $ readFile path
  let opts' = opts -- foldr applyHashDefine opts (view hashDefines opts)
      opts'' = over cppOptions turnOffLocations opts'
  hPutStrLn stderr ("loadModule " ++ show pair)
  (parsed', comments {-, _processed-}) <- Exts.fromParseResult <$> parseFileWithCommentsAndCPP (view cppOptions opts'') mode path
  let parsed = mapTopAnnotations (fixEnds comments moduleText) $ everywhere (mkT fixSpan) parsed'
  -- liftIO $ writeFile (path ++ ".cpp") processed
  -- putStr processed
  -- validateParseResults parsed comments processed -- moduleText
  key <- moduleKey path parsed
  -- putStrLn ("loaded " ++ prettyShow key)
  pure $ ModuleInfo { _moduleKey = key
                    , _module = parsed
                    , _moduleComments = comments
                    , _modulePath = makeRelative (case key of
                                                    ModuleKey {_moduleTop = top} -> top
                                                    ModuleFullPath p -> takeDirectory p) path
                    , _moduleText = moduleText
                    , _moduleSpan = spanOfText path moduleText
                    , _moduleGlobals = mempty }
-- | Compute the module key from a filepath (absolute or relative to
-- ".") and the parsed module.
moduleKey :: FilePath -> Module SrcSpanInfo -> IO ModuleKey
moduleKey _ (XmlPage {}) = error "XmlPage"
moduleKey _ (XmlHybrid {}) = error "XmlHybrid"
moduleKey path (Module _ Nothing _ _ _) = do
  ModuleFullPath <$> canonicalizePath path
moduleKey path (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = do
  canonicalizePath path >>= pure . makeKey
    where
      makeKey path' =
          let name' = splitModuleName name
              (path'', ext) = splitExtension path'
              dirs = splitDirectories path''
              (dirs', name'') = splitAt (length dirs - length name') dirs in
          case (name'' == name') of
            False -> ModuleFullPath path'
            True -> ModuleKey {_moduleTop = joinPath dirs', _moduleName = ModuleName () (intercalate "." name''), _moduleExt = ext}
      splitModuleName = filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))
