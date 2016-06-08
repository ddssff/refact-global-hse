{-# LANGUAGE TemplateHaskell #-}
import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Generics
import Data.Set as Set (insert)
import Debug.Trace
import Language.Haskell.Exts.Annotated
import Types (loadModule, ModuleInfo(..))
-- import Text.PrettyPrint.HughesPJClass (prettyShow)
import SrcLoc
import Symbols (foldDeclared, toExportSpecs)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (errors, failures, runTestTT, Test(TestList))
import Utils (withCurrentDirectory)

data Params
    = Params { _topDir :: FilePath
             , _paths :: [FilePath]
             }

params0 :: Params
params0 = Params {_topDir = ".", _paths = []}

$(makeLenses ''Params)

options :: [OptDescr (Params -> Params)]
options =
    [ Option "" ["top"] (ReqArg (\s -> over topDir (const s)) "DIR") "Set the top directory, module paths will be relative to this (so do it first)"
    , Option "" ["path"] (ReqArg (\s -> over paths (s :)) "PATH") "Path to a module to be parsed" ]

buildParams :: IO Params
buildParams = do
  args <- getArgs
  case getOpt' Permute options args of
    (fns, [], [], []) -> pure $ foldr ($) params0 fns
    _ -> error (usageInfo "Specify modules to parse.  Output appears in <Modulename>.syntax and <Modulename>.symbols" options)

-- | Parse a module and write its ast to /tmp/syntax
main :: IO ()
main = do
  params <- buildParams
  withCurrentDirectory (view top params) $
      loadModule' (view paths params) >>= mapM_ (\path -> either (doError path) (doModule path))
    where
      doError :: FilePath -> SomeException -> IO ()
      doError path e = hPutStrLn stderr $ "Failed to load " ++ show path ++ ": " ++ show e
      doModule path info@(ModuleInfo {_module = Module _ _ _ _ ds}) = do
         writeFile (dropExtension path <.> "syntax") (show (_module info))
         writeFile (dropExtension path <.> "exports") (unlines . map prettyPrint . concatMap toExportSpecs) ds
      doModule path info@(ModuleInfo {_module = m}) = error $ "Unexpected module: " ++ show m
{-
  Right info <- withCurrentDirectory "tests/input" $ loadModule "Menagerie.hs" :: IO (Either SomeException ModuleInfo)
  (writeFile "/tmp/syntax" . show . _module) info
  let Module _ _ _ _ ds = _module info
  (writeFile "/tmp/exports" . unlines . map prettyPrint . concatMap toExportSpecs) ds
  putStrLn "Saved AST to /tmp/syntax"
  -- putStrLn . show . filter (\x -> srcSpanEndColumn x == 0) $ (gFind (_module info) :: [SrcSpan])
-}
