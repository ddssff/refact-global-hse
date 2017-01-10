-- Example:
-- runhaskell scripts/Clean.hs --top=tests --top=. --top=scripts --mod=scripts/Tests.hs --unsafe

{-# LANGUAGE DeriveGeneric, RankNTypes, TemplateHaskell, TupleSections #-}

module CleanImports
    ( Params(..)
    , ghcOpts, findDirs, toClean, unsafe
    , options
    , go
    , finalParams
    ) where

import Control.Lens (_2, makeLenses, over, set, view)
import Data.Default (Default(def))
import Data.Set as Set (fromList, Set, toList, union)
import Language.Haskell.Names (Scoped(Scoped))
import Language.Preprocessor.Cpphs (runCpphsReturningSymTab)
import Options.Applicative (help, many, metavar, Parser, strOption, switch, long, (<>))
-- import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt', OptDescr(..), usageInfo)
import Refactor.Clean (cleanImports)
import Refactor.CPP (GHCOpts, ghcOptsOptions, cppOptions, definesL, hsSourceDirs)
import Refactor.LoadModule (loadModules)
import Refactor.Utils (withCleanRepo, withTempDirectory)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.FilePath (makeRelative)
import System.FilePath.Find ((&&?), (==?), always, depth, extension, fileType, FileType(RegularFile), find)

data Params
    = Params { _ghcOpts :: GHCOpts
             , _findDirs :: [FilePath]
             -- ^ Directories to search (non-recursively) for
             -- moduverse modules.
             , _toClean :: [(Maybe FilePath, FilePath)]
             -- ^ The modules we want to clean.  The Maybe FilePath
             -- is the hs-source-dirs path below which the module is
             -- located.
             , _unsafe :: Bool
             -- ^ If True operations can proceed even if there are
             -- uncommitted edits in the git repository.
             } deriving (Show)

$(makeLenses ''Params)

options :: Parser Params
options =
    Params <$> g <*> f <*> m <*> u
    where
      g :: Parser GHCOpts
      g = ghcOptsOptions
      f :: Parser [FilePath]
      f = many (strOption (long "find" <> metavar "DIR" <> help "Add modules found (non-recursively) in a directory to be cleaned"))
      m :: Parser [(Maybe FilePath, FilePath)]
      m = map (Nothing,) <$> many (strOption (long "mod" <> metavar "PATH" <> help "Add a module to the moduverse."))
      u :: Parser Bool
      u = switch (long "unsafe" <> help "Skip the safety check - allow uncommitted edits in repo where clean is performed")

params0 :: Params
params0 = Params {_ghcOpts = def, _findDirs = [], _toClean = [], _unsafe = False}

instance Default Params where
    def = params0

finalParams :: String -> Params -> IO Params
finalParams hfile params0 = do
  -- Canonicalize the search path
  params' <- mapM canonicalizePath (view findDirs params0) >>= \topDirs' -> return $ set findDirs topDirs' params0
  defs <- (fmap (over _2 (filter (/= '\n'))) . snd) <$> runCpphsReturningSymTab (view (ghcOpts . cppOptions) params') "cabal_macros.h" hfile
  let params = over (ghcOpts . cppOptions . definesL) (++ defs) params'

  -- Find all the haskell source files in all the search paths
  findModules <- concat <$> mapM (\dir -> find always {-(depth ==? 0)-} (extension ==? ".hs" &&? fileType ==? RegularFile) dir) (view findDirs params)
  abspaths <- mapM canonicalizePath findModules
  -- The absolute pathnames of the elements of the haskell source path
  abstops <- mapM canonicalizePath (view (ghcOpts . hsSourceDirs) params)
  -- Match up each module with the top relative to which its module
  -- name matches its path.  Some modules will appear below more than
  -- one top, in that case we need to match up the module name with
  -- the relative path.
  relModules <- modulePairs (view (ghcOpts . hsSourceDirs) params)
  -- Add all the modules we found to the list of modules to be cleaned (why???)
  --pure (over modules (++ (Set.toList relModules)) params)
  return params

-- | Return all the (top, module) pairs reachable from the list of
-- tops.  Note that some of these pairs will be invalid (as shown in
-- test1) because the module name does not match the relative path.
-- We can't eliminate these until we parse the module text.
modulePairs :: [FilePath] -> IO (Set (Maybe FilePath, FilePath))
modulePairs reltops = do
  abstops <- mapM canonicalizePath reltops
  relpaths <- concat <$> mapM (\dir -> find always (extension ==? ".hs" &&? fileType ==? RegularFile) dir) abstops
  abspaths <- mapM canonicalizePath relpaths
  foldr union mempty <$>
     mapM (\abspath -> do
             let relpaths = filter ((/= abspath) . snd) $
                              map (\(abstop, reltop) -> (Just reltop, makeRelative abstop abspath)) (zip abstops reltops)
             case relpaths of
               [] -> error $ "Module not found: " ++ show abspath
               xs -> pure (Set.fromList xs)) abspaths

go :: Params -> IO ()
go params0 = do
  params <- do
    macros <- readFile "dist/build/autogen/cabal_macros.h"
    finalParams macros params0
  (if (view unsafe params) then id else withCleanRepo) $ withTempDirectory True "." "scratch" $ \scratch -> do
    modules <- loadModules def (view toClean params)
    cleanImports [{-set hsSourceDirs (view (ghcOpts . hsSourceDirs) params)-} (view ghcOpts params)] (map (fmap (\(Scoped _ x) -> x)) modules)
