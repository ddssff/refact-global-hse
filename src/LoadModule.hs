{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}

module LoadModule
    ( loadModule
    , loadModules
    , Annot
    ) where

import Control.Lens (view)
import CPP (extensionsForHSEParser, GHCOpts, applyHashDefine, enabled, hashDefines)
import qualified CPP (defaultCpphsOptions)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate)
import Debug.Trace (trace)
import Language.Haskell.Exts.CPP (parseFileWithCommentsAndCPP)
import Language.Haskell.Exts.Syntax (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Names (annotate, resolve, Scoped(..))
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Preprocessor.Cpphs (BoolOptions(locations), CpphsOptions(..))
import ModuleInfo (ModuleInfo(..))
import ModuleKey (ModuleKey(..))
import SrcLoc (fixEnds, fixSpan, mapTopAnnotations, spanOfText)
import System.Directory (canonicalizePath)
import System.FilePath (joinPath, makeRelative, splitDirectories, splitExtension, takeDirectory)
import Utils (EZPrint(ezPrint))

type Annot = Scoped SrcSpanInfo

instance EZPrint Annot where
    ezPrint (Scoped _ x) = ezPrint x

-- | Load a list of modules and compute their global scoping info.
loadModules :: GHCOpts -> [FilePath] -> IO [ModuleInfo Annot]
loadModules opts paths = do
  t1 <$> addScoping <$> mapM (loadModule opts) paths
    where
      t1 :: [ModuleInfo l] -> [ModuleInfo l]
      t1 modules = trace ("modules loaded: " ++ show (map ezPrint modules)) modules

addScoping :: [ModuleInfo SrcSpanInfo] -> [ModuleInfo (Scoped SrcSpanInfo)]
addScoping mods =
    map (\m -> m {_module = annotate env (_module m),
                  _moduleGlobals = moduleTable (importTable env (_module m)) (_module m)}) mods
    where env = resolve (map _module mods) mempty

loadModule :: GHCOpts -> FilePath -> IO (ModuleInfo SrcSpanInfo)
loadModule opts path = do
  moduleText <- liftIO $ readFile path
  let cpphsOptions' = foldr applyHashDefine cpphsOptions (view hashDefines opts)
  (parsed', comments) <- Exts.fromParseResult <$> parseFileWithCommentsAndCPP cpphsOptions' mode path
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
    where
      mode = Exts.defaultParseMode {Exts.extensions = map EnableExtension (view enabled opts ++ extensionsForHSEParser),
                                    Exts.parseFilename = path,
                                    Exts.fixities = Nothing }

-- | Turn of the locations flag.  This means simple #if macros will not
-- affect the line numbers of the output text, so we can use the
-- resulting SrcSpan info on the original text.  Macro expansions
-- could still mess this up.
cpphsOptions :: CpphsOptions
cpphsOptions =
    CPP.defaultCpphsOptions
    { boolopts =
          ( boolopts CPP.defaultCpphsOptions )
          { locations = False
      }
    }

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
