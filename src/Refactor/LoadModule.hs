{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}

module Refactor.LoadModule
    ( ModuleFilePath(..)
    , loadModule
    , loadModules
    , addScope
    , unScope
    , test
    ) where

import Control.Lens (over, view)
import Control.Monad (join)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Default (def)
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate, nub)
import Data.Set as Set (member)
import Debug.Trace (trace)
import Language.Haskell.Exts (Name(Ident))
import Language.Haskell.Exts.CPP (parseFileWithCommentsAndCPP)
import Language.Haskell.Exts.Syntax (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Parser as Exts (fromParseResult, ParseMode(extensions, parseFilename, fixities))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Names (annotate, resolve, Scoped(..), Symbol(..))
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Preprocessor.Cpphs (BoolOptions(locations), CpphsOptions(..))
import Refactor.CPP (applyHashDefine, applyHashDefine', cppOptions, defaultParseMode, enabled, extensionsForHSEParser,
                     GHCOpts, hashDefines, turnOffLocations)
import qualified Refactor.CPP (defaultCpphsOptions)
import Refactor.ModuleInfo -- (ModuleInfo(..))
import Refactor.ModuleKey (ModuleKey(..))
import Refactor.SrcLoc (fixEnds, fixSpan, mapTopAnnotations, spanOfText)
import Refactor.Utils (EZPrint(ezPrint))
import System.Directory (canonicalizePath)
import System.FilePath ((</>), (<.>), joinPath, makeRelative, splitDirectories, splitExtension, takeDirectory)
import System.FilePath.Find as FilePath
import System.IO (hPutStrLn, stderr)

data ModuleFilePath =
  ModuleFilePath
  { _topDir :: Maybe FilePath
  , _subDir :: FilePath
  } deriving (Eq, Ord, Show)

instance EZPrint (Scoped SrcSpanInfo) where
    ezPrint (Scoped _ x) = ezPrint x

-- | Load a list of modules and compute their global scoping info.  As
-- of right now the scoping information is not actually used, but
-- surely there will be some use for it down the road.
--
--     λ> [i] <- fmap unScope <$> loadModules def [ModuleFilePath (Just "src") "Refactor.hs"]
--     λ> putStrLn (take 1000 (exactPrint' i))
--     module Refactor
--         ( ...
loadModules :: GHCOpts -> [ModuleFilePath] -> IO [ModuleInfo (Scoped SrcSpanInfo)]
loadModules opts pairs = addScope <$> mapM (loadModule opts) (nub pairs)

-- | Turn a ModuleInfo back into a ModuleFilePath.
moduleFilePath :: ModuleInfo l -> ModuleFilePath
moduleFilePath (ModuleInfo {_moduleKey = ModuleKey {_moduleTop = top, _moduleName = name, _moduleExt = ext}}) =
  ModuleFilePath (Just top) (subDir name ext)
  where
    subDir (ModuleName _ s) ext =
        (join $ filter (not . all (== '.')) $ groupBy (\a b -> (a == '.') == (b == '.')) s) <.> ext

-- | Scan a directory for haskell modules and return their ModuleFilePaths.
--     λ> moduleFilePaths "src"
--     [ModuleFilePath {_topDir = Just "src", _subDir = "Refactor/CPP.hs"},
--      ModuleFilePath {_topDir = Just "src", _subDir = "Refactor/Clean.hs"} ... ]
moduleFilePaths :: FilePath -> IO [ModuleFilePath]
moduleFilePaths top =
    fmap (\p -> ModuleFilePath (Just top) (makeRelative top p)) <$> FilePath.find always (extension ==? ".hs" &&? fileType ==? RegularFile) top

pairPath :: ModuleFilePath -> FilePath
pairPath (ModuleFilePath {_topDir = mtop, _subDir = path}) = maybe path (\top -> top </> path) mtop

-- | Load a single module:
--
--     λ> loadModule def (ModuleFilePath (Just "src") "Refactor.hs")
--     ModuleInfo {_moduleKey = ModuleKey {_moduleTop = "/home/dsf/git/refact-global-hse/src" ... }}
loadModule :: GHCOpts -> ModuleFilePath -> IO (ModuleInfo SrcSpanInfo)
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

prop_load_path :: ModuleFilePath -> IO Bool
prop_load_path p = ((== p) . moduleFilePath) <$> loadModule def p

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

test = do
  [i] <- loadModules def [ModuleFilePath (Just "src") "Refactor/Graph.hs"]
  -- trace (show i) (return ())
  let s = Value {symbolModule = ModuleName () "Refactor.Graph", symbolName = Language.Haskell.Exts.Ident () "findModuleByKeyUnsafe"}
  putNewModules (fmap unScope i) (fmap (fmap unScope) (decomposeModule i))
