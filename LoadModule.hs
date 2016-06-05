{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module LoadModule
    ( loadModule
    , loadModule'
    , loadModules
    , Annot
    , addScoping
    -- , hseExtensions
    -- , hsFlags
    -- , hsSourceDirs
    ) where

import qualified CPP (BoolOptions(locations), CpphsOptions(boolopts), defaultCpphsOptions, parseFileWithCommentsAndCPP)
import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted as IO (try)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate)
import Debug.Trace (trace)
import GHC (extensionsForHSEParser, GHCOpts(..))
import Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Exts.Extension (Extension(EnableExtension))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions, parseFilename, fixities), fromParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Names (annotate, resolve, Scoped)
import Language.Haskell.Names.Imports (importTable)
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import ModuleInfo (ModuleInfo(..))
import ModuleKey (ModuleKey(..))
import SrcLoc (fixSpan, mapTopAnnotations, fixEnds, spanOfText)
import System.Directory (canonicalizePath)
import System.FilePath (joinPath, makeRelative, splitDirectories, splitExtension, takeDirectory)
-- import Text.PrettyPrint.HughesPJClass as PP (prettyShow)
import Utils (EZPrint(ezPrint))

type Annot = Scoped SrcSpanInfo

loadModules :: GHCOpts -> [FilePath] -> IO [ModuleInfo Annot]
loadModules opts paths = do
  t1 <$> addScoping <$> mapM (loadModule' opts) paths
    where
      t1 :: [ModuleInfo l] -> [ModuleInfo l]
      t1 modules = trace ("modules loaded: " ++ show (map ezPrint modules)) modules

addScoping :: [ModuleInfo SrcSpanInfo] -> [ModuleInfo (Scoped SrcSpanInfo)]
addScoping mods =
    map (\m -> m {_module = annotate env (_module m),
                  _moduleGlobals = moduleTable (importTable env (_module m)) (_module m)}) mods
    where env = resolve (map _module mods) mempty

loadModule' :: GHCOpts -> FilePath -> IO (ModuleInfo SrcSpanInfo)
loadModule' opts path = either (error . show) id <$> (loadModule opts path :: IO (Either SomeException (ModuleInfo SrcSpanInfo)))

loadModule :: Exception e => GHCOpts -> FilePath -> IO (Either e (ModuleInfo SrcSpanInfo))
loadModule opts path = try $ do
  moduleText <- liftIO $ readFile path
  (parsed', comments, _processed) <- Exts.fromParseResult <$> CPP.parseFileWithCommentsAndCPP cpphsOptions mode path
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
      mode = Exts.defaultParseMode {Exts.extensions = map EnableExtension (GHC.extensions opts ++ extensionsForHSEParser),
                                    Exts.parseFilename = path,
                                    Exts.fixities = Nothing }
  -- {- `IO.catch` (\(e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e) -}

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
  canonicalizePath path >>= pure . makeKey
    where
      makeKey path' =
          let name' = splitModuleName name
              (path'', ext) = splitExtension path'
              dirs = splitDirectories path''
              (dirs', name'') = splitAt (length dirs - length name') dirs in
          case (name'' == name') of
            False -> ModuleFullPath path'
            True -> ModuleKey {_moduleTop = joinPath dirs', _moduleName = A.ModuleName () (intercalate "." name''), _moduleExt = ext}
      splitModuleName = filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))
