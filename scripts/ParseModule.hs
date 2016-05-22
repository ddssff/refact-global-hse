import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Generics
import Data.Set as Set (insert)
import Debug.Trace
import IO (withCurrentDirectory)
import Language.Haskell.Exts.Annotated
import Types (loadModule, ModuleInfo(..))
-- import Text.PrettyPrint.HughesPJClass (prettyShow)
import SrcLoc
import Symbols (foldDeclared)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (errors, failures, runTestTT, Test(TestList))

-- | Parse a module and write its ast to /tmp/syntax
main :: IO ()
main = do
  Right info <- withCurrentDirectory "tests/input/decl-mover" $ loadModule "IO.hs" :: IO (Either SomeException ModuleInfo)
  (writeFile "/tmp/syntax" . show . _module) info
  putStrLn "Saved AST to /tmp/syntax"
  -- putStrLn . show . filter (\x -> srcSpanEndColumn x == 0) $ (gFind (_module info) :: [SrcSpan])
