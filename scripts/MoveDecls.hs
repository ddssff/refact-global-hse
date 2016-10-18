-- Example:
-- runhaskell scripts/Move.hs --move=FoldDeclared,Symbols,Tmp --unsafe

{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}

module MoveDecls
    ( Params(..)
    , options
    , go
    ) where

import Control.Lens (makeLenses, over, set, view)
import Control.Monad (foldM)
import CPP (GHCOpts, ghcOptsOptions, hsSourceDirs)
import Data.Default (def)
import Data.List (groupBy)
import Data.Monoid ((<>))
import Data.Tuple.HT (uncurry3)
import Decls (moveDeclsAndClean)

import MoveSpec (instClassPred, splicePred)
import LoadModule (loadModules)
import MoveSpec (moveDeclsByName, moveInstDecls, moveSpliceDecls, MoveSpec, traceMoveSpec)
import Options.Applicative (eitherReader, help, long, many, metavar, option, Parser, ReadM, strOption, switch, value)
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt', OptDescr(..), usageInfo)
import System.Environment (getArgs)
import System.FilePath ((</>), makeRelative)
import System.FilePath.Find ((&&?), (==?), depth, extension, fileType, FileType(RegularFile), find)
import Utils (gitResetSubdir, withCleanRepo, withTempDirectory, withCurrentDirectory)

#if !MIN_VERSION_optparse_applicative(0,13,0)
-- | Convert a function in the 'Maybe' monad to a reader.
maybeReader :: (String -> Maybe a) -> ReadM a
maybeReader f = eitherReader $ \arg ->
  maybe (Left $ "cannot parse value `" ++ arg ++ "'") pure . f $ arg
#endif

data Params
    = Params { _moveSpec :: MoveSpec
             , _cd :: FilePath
             , _ghcOpts :: GHCOpts
             , _lsDirs :: [FilePath]
             , _findDirs :: [FilePath]
             , _moduverse :: [FilePath]
             , _unsafe :: Bool
             , _gitReset :: Bool }
      deriving Show

$(makeLenses ''Params)

options :: Parser Params
options =
    Params
    <$> ms
    <*> cd
    <*> gco
    <*> lds
    <*> fds
    <*> mverse
    <*> us
    <*> gr
    where
      ms :: Parser MoveSpec
      ms =
          let ds :: Parser MoveSpec
              ds = mconcat <$> (many (uncurry3 moveDeclsByName <$> option (maybeReader splitTriple) (long "decl" <> metavar "SYMBOL,DEPARTMOD,ARRIVEMOD" <> help "Move the declaration of a symbol")))
              is :: Parser MoveSpec
              is = mconcat <$> (many ((moveInstDecls . uncurry3 instClassPred) <$> option (maybeReader splitTriple) (long "inst" <> metavar "CLASSNAME,DEPARTMOD,ARRIVEMOD" <> help "Move all instances of a class")))
              ss :: Parser MoveSpec
              ss = mconcat <$> many ((moveSpliceDecls . uncurry3 splicePred) <$> option (maybeReader splitTriple) (long "splice" <> metavar "SYMBOL,DEPARTMOD,ARRIVEMOD" <> help "Move all splices that reference a symbol")) in
          (<>) <$> ds <*> ((<>) <$> is <*> ss)
      cd = strOption (value "." <> long "cd" <> metavar "DIR" <> help "Set the process working directory")
      gco = ghcOptsOptions
      lds = many (strOption (long "ls"<> metavar "DIR" <> help "Directory relative to top to search (non-recursively) for .hs files to add to the moduverse"))
      fds = many (strOption (long "find" <> metavar "DIR" <> help "Directory relative to top to search (recursively) for .hs files to add to the moduverse"))
      mverse = many (strOption (long "mod" <> metavar "PATH" <> help "Add a module to the moduverse"))
      us = switch (long "unsafe" <> help "Skip the safety check - allow uncommitted edits in repo where clean is performed")
      gr = switch (long "reset" <> help "Do a hard reset and git clean on the working directory (requires --unsafe)")

#if 0
params0 :: Params
params0 = Params {_moveSpec = mempty, _gitReset = False,_cd = ".", _ghcOpts = def, _findDirs = [], _lsDirs = [], _moduverse = [], _unsafe = False}

options' :: [OptDescr (Params -> Params)]
options' =
    [ Option "" ["decl"] (ReqArg (\s -> maybe (error s)
                                              (\(name, depart, arrive) -> over moveSpec ((<>) (moveDeclsByName name depart arrive)))
                                              (splitTriple s)) "SYMBOL,DEPARTMOD,ARRIVEMOD")
             "Move the declaration of a symbol"
    , Option "" ["inst"] (ReqArg (\s -> maybe (error s)
                                              (\(classname, depart, arrive) -> over moveSpec ((<>) (moveInstDecls (instClassPred classname depart arrive))))
                                              (splitTriple s)) "CLASSNAME,DEPARTMOD,ARRIVEMOD")
             "Move all instances of a class"
    , Option "" ["splice"] (ReqArg (\s -> maybe (error s)
                                                (\(name,depart,arrive) -> over moveSpec ((<>) (moveSpliceDecls (splicePred name depart arrive))))
                                                (splitTriple s)) "SYMBOL,DEPARTMOD,ARRIVEMOD")
             "Move all splices that reference a symbol"
    , Option "" ["mod"] (ReqArg (\s -> over moduverse (s :)) "PATH") "Add a module to the moduverse"
    , Option "" ["cd"] (ReqArg (\s -> over cd (const s)) "DIR") "Set the process working directory"
    , Option "i" ["hs-source-dir"] (ReqArg (\s -> over (ghcOpts . hsSourceDirs) (s :)) "DIR") "Add a directory to the haskell source path"
    , Option "" ["ls"] (ReqArg (\s -> over lsDirs (s :)) "DIR") "Directory relative to top to search (non-recursively) for .hs files to add to the moduverse"
    , Option "" ["find"] (ReqArg (\s -> over findDirs (s :)) "DIR") "Directory relative to top to search (recursively) for .hs files to add to the moduverse"
    , Option "" ["unsafe"] (NoArg (set unsafe True)) "Skip the safety check - allow uncommitted edits in repo where clean is performed"
    , Option "" ["reset"] (NoArg (set gitReset True)) "Do a hard reset and git clean on the working directory (requires --unsafe)" ]

buildParams :: [String] -> IO Params
buildParams args = do
  case getOpt' Permute options' args of
    (fns, [], [], []) -> finalize (foldr ($) params0 fns)
    (x, y, z, w) -> error (usageInfo ("error: " ++ show (y, z, w) ++ "\nspecify modules and at least one move spec") (options' :: [OptDescr (Params -> Params)]))
    where
#endif

-- Search the findDir directories for paths and add them to moduverse.
finalize :: Params -> IO Params
finalize params = withCurrentDirectory (_cd params) $ do
  paths1 <- mapM (\dir -> {-map (makeRelative (view topDir params))
                                      <$>-} (find (depth ==? 0)
                                                  (extension ==? ".hs" &&? fileType ==? RegularFile)
                                                  ({-view topDir params </>-} dir)))
                 (_lsDirs params)
  paths2 <- mapM (\dir -> {-map (makeRelative (view topDir params))
                                      <$>-} (find (depth ==? 0)
                                                  (extension ==? ".hs" &&? fileType ==? RegularFile)
                                                  ({-view topDir params </>-} dir)))
                 (_findDirs params)
  pure $ over moduverse (++ (concat (paths1 ++ paths2))) params

go :: Params -> IO ()
go params' = do
  params <- finalize params'
  withCurrentDirectory (_cd params) $ maybeReset params $ do
    modules <- loadModules def (view moduverse params)
    moveDeclsAndClean (view moveSpec params) (view ghcOpts params) modules
    where
      maybeReset :: Params -> IO () -> IO ()
      maybeReset params = if _unsafe params then (if _gitReset params then (\action -> gitResetSubdir "." >> action)  else id) else withCleanRepo

splitTriple :: String -> Maybe (String, String, String)
splitTriple s =
    case filter (not . elem ',') (groupBy (\a b -> (a == ',') == (b == ',')) s) of
      [a,b,c] -> Just (a, b, c)
      _ -> Nothing
