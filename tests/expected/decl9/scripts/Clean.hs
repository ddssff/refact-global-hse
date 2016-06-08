-- Example:
-- runhaskell scripts/Clean.hs --top=tests --top=. --top=scripts --mod=scripts/Tests.hs --unsafe

{-# LANGUAGE RankNTypes, TemplateHaskell #-}
import Control.Lens
import Control.Monad (foldM)
import Data.Default (def)
import Data.List (groupBy, stripPrefix)
import Data.Monoid ((<>))
import Debug.Trace
import GHC (hsSourceDirs)
import Imports (cleanImports)
import Language.Haskell.Names (Scoped(Scoped))
import LoadModule (loadModules)
import Utils (withCleanRepo, withCurrentDirectory, withTempDirectory)
import System.Directory
import System.Environment
import System.FilePath ((</>), makeRelative)
import System.FilePath.Find ((&&?), (==?), always, depth, extension, fileType, FileType(RegularFile), find)
import System.Console.GetOpt

data Params
    = Params { _topDirs :: [FilePath]
             , _findDirs :: [FilePath]
             , _moduverse :: [FilePath]
             , _unsafe :: Bool }

$(makeLenses ''Params)

params0 :: Params
params0 = Params {_topDirs = [], _findDirs = [], _moduverse = [], _unsafe = False}

options :: [OptDescr (Params -> Params)]
options =
    [ Option "" ["mod"] (ReqArg (\s -> over moduverse (s :)) "PATH") "Add a module to the moduverse"
    , Option "" ["top"] (ReqArg (\s -> over topDirs (s :)) "DIR") "Add a top directory, like the cabal hs-source-dirs option"
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
  topDirs' <- mapM canonicalizePath (view topDirs params)
  findDirs' <- mapM canonicalizePath (view findDirs params)
  let params' = set topDirs topDirs' $  set findDirs findDirs' $ params

  -- The elements of moduverse are paths relative to ".",
  -- now figure out which top they are 
  -- we want t
  findModules <- concat <$> mapM (\dir -> find (depth ==? 0) (extension ==? ".hs" &&? fileType ==? RegularFile) dir) (view findDirs params')
  findModules' <- mapM canonicalizePath findModules
  relModules <- mapM (\path -> do
                        let relpaths = filter (/= path) (map (\top -> makeRelative top path) (view topDirs params'))
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
    cleanImports scratch (def {hsSourceDirs = view topDirs params}) (map (fmap (\(Scoped _ x) -> x)) modules)
