-- Example:
-- runhaskell scripts/Clean.hs --top=tests --top=. --top=scripts --mod=scripts/Tests.hs --unsafe

{-# LANGUAGE RankNTypes, TemplateHaskell #-}

import Clean (cleanImports)
import CPP (GHCOpts, hsSourceDirs)
import Control.Lens (makeLenses, over, set, view)
import Data.Default (def)
import Language.Haskell.Names (Scoped(Scoped))
import LoadModule (loadModules)
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt', OptDescr(..), usageInfo)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.FilePath (makeRelative)
import System.FilePath.Find ((&&?), (==?), depth, extension, fileType, FileType(RegularFile), find)
import Utils (withCleanRepo, withTempDirectory)

data Params
    = Params { _ghcOpts :: GHCOpts
             , _findDirs :: [FilePath]
             , _moduverse :: [FilePath]
             , _unsafe :: Bool }

$(makeLenses ''Params)

params0 :: Params
params0 = Params {_ghcOpts = def, _findDirs = [], _moduverse = [], _unsafe = False}

options :: [OptDescr (Params -> Params)]
options =
    [ Option "" ["mod"] (ReqArg (\s -> over moduverse (s :)) "PATH") "Add a module to the moduverse"
    , Option "" ["top"] (ReqArg (\s -> over (ghcOpts . hsSourceDirs) (s :)) "DIR") "Add a top directory, like the cabal hs-source-dirs option"
    , Option "" ["find"] (ReqArg (\s -> over findDirs (s :)) "DIR") "Add modules found (non-recursively) in a directory"
    , Option "" ["unsafe"] (NoArg (set unsafe True)) "Skip the safety check - allow uncommitted edits in repo where clean is performed" ]

buildParams :: IO Params
buildParams = do
  args <- getArgs
  case getOpt' Permute options args of
    (fns, [], [], []) -> pure $ foldr ($) params0 fns
    _ -> error (usageInfo "specify modules and at least one move spec" options)

finalParams :: Params -> IO Params
finalParams params = do
  params' <- mapM canonicalizePath (view (ghcOpts . hsSourceDirs) params) >>= \topDirs' -> return $ set (ghcOpts . hsSourceDirs) topDirs' params
  mapM canonicalizePath (view findDirs params') >>= \topDirs' -> return $ set findDirs topDirs' params'

  -- The elements of moduverse are paths relative to ".",
  -- now figure out which top they are 
  -- we want t
  findModules <- concat <$> mapM (\dir -> find (depth ==? 0) (extension ==? ".hs" &&? fileType ==? RegularFile) dir) (view findDirs params')
  findModules' <- mapM canonicalizePath findModules
  relModules <- mapM (\path -> do
                        let relpaths = filter (/= path) (map (\top -> makeRelative top path) (view (ghcOpts . hsSourceDirs) params'))
                        case relpaths of
                          [] -> error $ "Module not found: " ++ path
                          [x] -> pure x
                          xs -> error $ "Multiple modules found: " ++ show xs) findModules'
  pure (over moduverse (++ relModules) params')
{-
    where
      -- Search the findDir directories for paths and add them to moduverse.
      finalize :: Params -> IO Params
      finalize params = do
        -- Find all the module paths
        let paths1 = view moduverse params
        paths2 <- mapM (\dir -> mapM (find (depth ==? 0) (extension ==? ".hs" &&? fileType ==? RegularFile) dir)) (view findDirs params) >>= mapM moduleRelPath . concat
        pure (paths1 ++ paths2)
      findModule :: FilePath -> IO FilePath
      findModule relpath = mapM (\top -> doesFileExist (top </> relpath) >>= bool Nothing (Just relpath

        paths <- concat <$> mapM (\dir ->
                                      map (makeRelative (view topDirs params </> dir))
                                              <$> (find (depth ==? 0)
                                                        (extension ==? ".hs" &&? fileType ==? RegularFile)
                                                        (view topDirs params </> dir)))
                                 (_findDirs params)
        pure $ over moduverse (++ paths) params
-}

main = do
  params <- buildParams >>= finalParams
  (if (view unsafe params) then id else withCleanRepo) $ withTempDirectory True "." "scratch" $ \scratch -> do
    modules <- loadModules def (view moduverse params)
    cleanImports [set hsSourceDirs (view (ghcOpts . hsSourceDirs) params) def] (map (fmap (\(Scoped _ x) -> x)) modules)
