-- Example:
-- runhaskell scripts/Clean.hs --top=tests --top=. --top=scripts --mod=scripts/Tests.hs --unsafe

{-# LANGUAGE DeriveGeneric, RankNTypes, TemplateHaskell #-}

module CleanImports
    ( Params(..)
    , options
    , go
    ) where

import Clean (cleanImports)
import CPP (GHCOpts, ghcOptsOptions, hsSourceDirs)
import Control.Lens (makeLenses, over, set, view)
import Data.Default (def)
import Language.Haskell.Names (Scoped(Scoped))
import LoadModule (loadModules)
import Options.Applicative (help, many, metavar, Parser, strOption, switch, long, (<>))
-- import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt', OptDescr(..), usageInfo)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.FilePath (makeRelative)
import System.FilePath.Find ((&&?), (==?), depth, extension, fileType, FileType(RegularFile), find)
import Utils (withCleanRepo, withTempDirectory)

data Params
    = Params { _ghcOpts :: GHCOpts
             , _findDirs :: [FilePath]
             , _moduverse :: [FilePath]
             , _unsafe :: Bool } deriving (Show)

$(makeLenses ''Params)

options :: Parser Params
options =
    Params <$> g <*> f <*> m <*> u
    where
      g :: Parser GHCOpts
      g = ghcOptsOptions
      f :: Parser [FilePath]
      f = many (strOption (long "find" <> metavar "DIR" <> help "Add modules found (non-recursively) in a directory to be cleaned"))
      m :: Parser [FilePath]
      m = many (strOption (long "mod" <> metavar "PATH" <> help "Add a module to the moduverse."))
      u :: Parser Bool
      u = switch (long "unsafe" <> help "Skip the safety check - allow uncommitted edits in repo where clean is performed")

params0 :: Params
params0 = Params {_ghcOpts = def, _findDirs = [], _moduverse = [], _unsafe = False}

finalParams :: Params -> IO Params
finalParams params = do
  mapM canonicalizePath (view findDirs params) >>= \topDirs' -> return $ set findDirs topDirs' params

  findModules <- concat <$> mapM (\dir -> find (depth ==? 0) (extension ==? ".hs" &&? fileType ==? RegularFile) dir) (view findDirs params)
  findModules' <- mapM canonicalizePath findModules
  relModules <- mapM (\path -> do
                        let relpaths = filter (/= path) (map (\top -> makeRelative top path) (view (ghcOpts . hsSourceDirs) params))
                        case relpaths of
                          [] -> error $ "Module not found: " ++ path
                          [x] -> pure x
                          xs -> error $ "Multiple modules found: " ++ show xs) findModules'
  pure (over moduverse (++ relModules) params)

go params0 = do
  params <- finalParams params0
  (if (view unsafe params) then id else withCleanRepo) $ withTempDirectory True "." "scratch" $ \scratch -> do
    modules <- loadModules def (view moduverse params)
    cleanImports [set hsSourceDirs (view (ghcOpts . hsSourceDirs) params) def] (map (fmap (\(Scoped _ x) -> x)) modules)
