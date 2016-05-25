-- Example:
-- runhaskell scripts/Move.hs --move=FoldDeclared,Symbols,Tmp --unsafe

{-# LANGUAGE RankNTypes, TemplateHaskell #-}
import Control.Lens
import Control.Monad (foldM)
import Data.List (groupBy, stripPrefix)
import Data.Monoid ((<>), mempty)
import Debug.Trace
import Decls (moveDeclsByName, moveDeclsAndClean, MoveSpec, traceMoveSpec)
import Types (loadModules)
import Utils (withCleanRepo, withCurrentDirectory, withTempDirectory)
import System.Environment
import System.FilePath ((</>), makeRelative)
import System.FilePath.Find ((&&?), (==?), always, depth, extension, fileType, FileType(RegularFile), find)
import System.Console.GetOpt

data Params
    = Params { _moveSpec :: MoveSpec
             , _topDir :: FilePath
             , _findDirs :: [FilePath]
             , _moduverse :: [FilePath]
             , _unsafe :: Bool }

$(makeLenses ''Params)

params0 :: Params
params0 = Params {_moveSpec = mempty, _topDir = ".", _findDirs = [], _moduverse = [], _unsafe = False}

options :: [OptDescr (Params -> Params)]
options =
    [ Option "" ["move"] (ReqArg (\s -> case filter (not . elem ',') (groupBy (\a b -> (a == ',') == (b == ',')) s) of
                                          [name, depart, arrive] -> over moveSpec ((<>) (moveDeclsByName name depart arrive))
                                          _ -> error s) "SYMNAME,DEPARTMOD,ARRIVEMOD")
             "Move the declaration of a symbol",
      Option "" ["mod"] (ReqArg (\s -> over moduverse (s :)) "PATH") "Add a module to the moduverse",
      Option "" ["top"] (ReqArg (\s -> over topDir (const s)) "DIR") "Set the top directory, module paths will be relative to this (so do it first)",
      Option "" ["find"] (ReqArg (\s -> over findDirs (s :)) "DIR") "Directory relative to top to search (non-recursively) for .hs files to add to the moduverse"
    , Option "" ["unsafe"] (NoArg (set unsafe True)) "Skip the safety check - allow uncommitted edits in repo where clean is performed" ]

buildParams :: [String] -> IO Params
buildParams args = do
  case getOpt' Permute options args of
    (fns, [], [], []) -> finalize (foldr ($) params0 fns)
    (x, y, z, w) -> error (usageInfo ("error: " ++ show (y, z, w) ++ "\nspecify modules and at least one move spec") options)
    where
      -- Search the findDir directories for paths and add them to moduverse.
      finalize :: Params -> IO Params
      finalize params = do
        paths <- concat <$> mapM (\dir ->
                                      map (makeRelative (view topDir params))
                                              <$> (find (depth ==? 0)
                                                        (extension ==? ".hs" &&? fileType ==? RegularFile)
                                                        (view topDir params </> dir)))
                                 (_findDirs params)
        pure $ over moduverse (++ paths) params

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = do
  params <- buildParams args
  (if _unsafe params then id else withCleanRepo) $ withTempDirectory True "." "scratch" $ \scratch -> do
    modules <- loadModules (view moduverse params)
    moveDeclsAndClean (traceMoveSpec (view moveSpec params)) scratch modules
