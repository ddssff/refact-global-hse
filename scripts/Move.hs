{-# LANGUAGE RankNTypes, TemplateHaskell #-}
import Control.Lens
import Control.Monad (foldM)
import Data.List (groupBy, stripPrefix)
import Data.Monoid ((<>))
import Debug.Trace
import Decls (appendMoveSpecs, identityMoveSpec, makeMoveSpec, moveDeclsAndClean, MoveSpec)
import Types (loadModules)
import IO (withCurrentDirectory, withTempDirectory)
import Utils (withCleanRepo)
import System.Environment
import System.FilePath ((</>), makeRelative)
import System.FilePath.Find ((&&?), (==?), always, depth, extension, fileType, FileType(RegularFile), find)
import System.Console.GetOpt

data Params
    = Params { _moveSpec :: MoveSpec
             , _topDir :: FilePath
             , _findDirs :: [FilePath]
             , _moduverse :: [FilePath] }

$(makeLenses ''Params)

params0 :: Params
params0 = Params {_moveSpec = identityMoveSpec, _topDir = ".", _findDirs = [], _moduverse = []}

options :: [OptDescr (Params -> Params)]
options =
    [ Option "" ["move"] (ReqArg (\s -> case filter (not . elem ',') (groupBy (\a b -> (a == ',') == (b == ',')) s) of
                                          [name, depart, arrive] -> over moveSpec (appendMoveSpecs (makeMoveSpec name depart arrive))
                                          _ -> error s) "SYMNAME,DEPARTMOD,ARRIVEMOD")
             "Move the declaration of a symbol",
      Option "" ["mod"] (ReqArg (\s -> over moduverse (s :)) "PATH") "Add a module to the moduverse",
      Option "" ["top"] (ReqArg (\s -> over topDir (const s)) "DIR") "Set the top directory, module paths will be relative to this (so do it first)",
      Option "" ["find"] (ReqArg (\s -> over findDirs (s :)) "DIR") "Directory relative to top to search (non-recursively) for .hs files to add to the moduverse" ]

buildParams = do
  args <- getArgs
  case getOpt' Permute options args of
    (fns, [], [], []) -> finalize (foldr ($) params0 fns)
    _ -> error (usageInfo "specify modules and at least one move spec" options)
    where
      -- Search the findDir directories for paths and add them to moduverse.
      finalize :: Params -> IO Params
      finalize params = do
        paths <- concat <$> mapM (\dir ->
                                      map (makeRelative (view topDir params </> dir))
                                              <$> (find (depth ==? 0)
                                                        (extension ==? ".hs" &&? fileType ==? RegularFile)
                                                        (view topDir params </> dir)))
                                 (_findDirs params)
        pure $ over moduverse (++ paths) params

main = do
  params <- buildParams
  withCleanRepo $ withTempDirectory True "." "scratch" $ \scratch -> do
    modules <- loadModules (view moduverse params)
    moveDeclsAndClean (traceMoveSpec (view moveSpec params)) scratch modules

traceMoveSpec :: MoveSpec -> MoveSpec
traceMoveSpec f = \k d -> let k' = f k d in if k /= k' then (trace ("moveSpec " ++ show k ++ " d -> " ++ show k') k') else k'
