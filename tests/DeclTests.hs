-- | Test the following cases for import moving
-- (1) Import of a symbol moves out of a module
-- (2) Import of a symbol that moves between two other modules
-- (3a) Import of a symbol that moves into a module, but is
--      still used by its old module
-- (3b) Import of a symbol that moves into a module, but is
--      no longer used by its old module

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module DeclTests where

import CPP (defaultCpphsOptions, GHCOpts(..))
import Control.Monad (when)
import Data.Data (Data)
import Data.List hiding (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Decls (runMoveUnsafe, runSimpleMoveUnsafe)
import HashDefine (parseHashDefine)
import Imports (cleanImports)
import Language.Haskell.Exts.Annotated.Simplify (sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(FunBind, TypeSig), Exp(App), Match(InfixMatch, Match), Module(Module), ModuleName(ModuleName), Name(Ident))
import Language.Haskell.Exts.Extension (KnownExtension(CPP, OverloadedStrings, ExtendedDefaultRules))
import Language.Haskell.Exts.SrcLoc (SrcInfo)
import qualified Language.Haskell.Exts.Syntax as S (Name(Ident))
import LoadModule (loadModule)
import ModuleInfo (ModuleInfo(ModuleInfo, _module, _moduleKey))
import ModuleKey (ModuleKey(ModuleKey, _moduleName), moduleName)
import MoveSpec (applyMoveSpec, moveDeclsByName, moveInstDecls, MoveSpec(MoveSpec), moveSpliceDecls)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertString, Test(..))
import Utils (EZPrint(ezPrint), gFind, gitResetSubdir, simplify, withCleanRepo, withCurrentDirectory)

declTests :: Test
declTests = TestList [decl1, decl2, decl3, decl4, decl5, decl6, {-decl7,-} decl8, decl9,
                      simple1, simple2, simple3]

-- Test moving a declaration to a module that currently imports it
decl1 :: Test
decl1 = TestLabel "decl1" $ TestCase $ testMoveSpec "tests/expected/decl1" "tests/input/atp-haskell" moveSpec1

-- Move tryfindM from Lib to Tableaux.  Tableaux already imports
-- tryfindM, so that import should be removed.  The question is, now
-- that tryfindM has moved out of Lib, can we or must we import
-- tryfindM it from Tableaux?  The answer is no, because of this loop:
--
--     Tableaux -> Quantified -> Prop -> Lib -> Tableaux.
--
-- The fact that Tableaux already imports Lib forces us to omit any
-- import of Tableaux from Lib.

moveSpec1 :: MoveSpec
moveSpec1 = MoveSpec f
    where
      f i (A.TypeSig _ [A.Ident _ s] _)
          | s == "tryfindM" {-|| s == "failing"-} =
              (_moduleKey i) {_moduleName = A.ModuleName () "Data.Logic.ATP.Tableaux"}
      f i (A.FunBind _ ms)
          | any (`elem` [S.Ident "tryfindM" {-, S.Ident "failing"-}])
                (map (\match -> case match of
                                  A.Match _ name _ _ _ -> sName name
                                  A.InfixMatch _ _ name _ _ _ -> sName name) ms) =
              (_moduleKey i) {_moduleName = A.ModuleName () "Data.Logic.ATP.Tableaux"}
      f i __ = _moduleKey i
{-
moveSpec1 k d | Set.member (S.Ident "tryfindM") (foldDeclared Set.insert mempty d) =
                  trace ("Expected TypeSig or FunBind: " ++ show d)
                        (k {_modulePath = "Data/Logic/ATP/Tableaux.hs",
                            _moduleName = S.ModuleName "Data.Logic.ATP.Tableaux"})
moveSpec1 k d@(A.FunBind _ _) = {-trace ("FunBind: " ++ show (foldDeclared (:) mempty d))-} k
moveSpec1 k (A.ClassDecl _ _mcxt _dh _fd _mcds) = k
moveSpec1 k (A.DataDecl _ _dn _mcxt _dh _qcs _md) = k
moveSpec1 k (A.PatBind _ _p _rhs _mbs) = k
moveSpec1 k (A.TypeDecl _ _dh _typ) = k
moveSpec1 k (A.TypeSig _ _name _typ) = k
moveSpec1 k (A.InstDecl _ _mo _ir _mids) = k
moveSpec1 k (A.InfixDecl _ _assoc _mi _ops) = k
moveSpec1 k (A.DerivDecl {}) = k
moveSpec1 k d = error $ "Unexpected decl: " ++ take 120 (show d) ++ ".."
-}

-- Test moving a declaration to a non-existant module
-- Test updating import of decl that moved from A to B in module C
decl2 :: Test
decl2 = TestLabel "decl2" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testMoveSpec' "tests/expected/decl2" input
            (runSimpleMoveUnsafe input (moveDeclsByName "withCurrentDirectory" "IO" "Tmp" :: MoveSpec))

-- Test moving a declaration to a module that does *not* currently
-- import it.  Now we don't know whether it leaves behind uses for
-- which we need to import it (which might be called "moving down") or
-- if we are moving it to the place where it is used (which could be
-- called "moving up".)
decl3 :: Test
decl3 = TestLabel "decl3" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testMoveSpec' "tests/expected/decl3" input
            (runSimpleMoveUnsafe input (moveDeclsByName "lines'" "SrcLoc" "Tmp" :: MoveSpec) >>
             runSimpleMoveUnsafe input (moveDeclsByName "lines'" "Tmp" "Utils" :: MoveSpec))

decl4 :: Test
decl4 = TestLabel "decl4" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testMoveSpec' "tests/expected/decl4" input (runSimpleMoveUnsafe input spec)
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "withTempDirectory" "IO" "Utils",
                           moveDeclsByName "ignoringIOErrors" "IO" "Utils",
                           moveDeclsByName "FoldDeclared" "Symbols" "Tmp",
                           moveInstDecls instpred]
      instpred i name _types
          | (gFind name :: [A.Name ()]) == [A.Ident () "FoldDeclared"] =
              (_moduleKey i) {_moduleName = A.ModuleName () "Tmp"}
      instpred i _ _ = _moduleKey i

decl5 :: Test
decl5 = TestLabel "decl5" $ TestCase $ testMoveSpec "tests/expected/decl5" "tests/input/decl-mover" spec
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "ModuleKey" "Types" "ModuleKey",
                           moveDeclsByName "fullPathOfModuleKey" "Types" "ModuleKey",
                           moveDeclsByName "moduleKey" "Types" "ModuleKey"]

decl6 :: Test
decl6 = TestLabel "decl6" $ TestCase $ testMoveSpec "tests/expected/decl6" "tests/input/decl-mover" spec
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "MoveSpec" "Decls" "MoveSpec",
                           moveDeclsByName "moveDeclsByName" "Decls" "MoveSpec",
                           moveDeclsByName "appendMoveSpecs" "Decls" "MoveSpec",
                           moveDeclsByName "identityMoveSpec" "Decls" "MoveSpec"]

-- Need a way to add imports of the lenses created by makeLenses
decl7 :: Test
decl7 = TestLabel "decl7" $ TestCase $ testMoveSpec' "tests/expected/decl7" "tests/input/rgh" $
          runMoveUnsafe "tests/input/rgh" [".", "tests"] spec
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "textOfSpan" "SrcLoc" "Scan",
                           moveDeclsByName "srcLoc" "SrcLoc" "Scan",
                           moveDeclsByName "endLoc" "SrcLoc" "Scan",
                           moveDeclsByName "splitText" "SrcLoc" "Scan",
                           moveDeclsByName "spanDiff" "SrcLoc" "Scan",
                           moveDeclsByName "testSpan" "SrcLoc" "Scan",
                           moveDeclsByName "SpanInfo" "SrcLoc" "Scan",
                           moveInstDecls instPred,
                           moveDeclsByName "ScanM" "SrcLoc" "Scan",
                           moveDeclsByName "locDiff" "SrcLoc" "Scan",
                           moveDeclsByName "locSum" "SrcLoc" "Scan",
                           moveDeclsByName "endLocOfText" "SrcLoc" "Scan",
                           moveDeclsByName "ScanM" "SrcLoc" "Scan",
                           moveDeclsByName "skip" "SrcLoc" "Scan",
                           moveDeclsByName "keep" "SrcLoc" "Scan",
                           moveDeclsByName "trailingWhitespace" "SrcLoc" "Scan",
                           moveDeclsByName "withTrailingWhitespace" "SrcLoc" "Scan",
                           moveDeclsByName "debugRender" "SrcLoc" "Scan",
                           moveDeclsByName "void" "SrcLoc" "Scan",
                           moveDeclsByName "St" "SrcLoc" "Scan",
                           moveSpliceDecls testSplice]
      -- testSplice key@(ModuleKey {_moduleName = S.ModuleName "SrcLoc"}) _ = key {_moduleName = S.ModuleName "Scan"}
      testSplice :: ModuleInfo () -> A.Exp () -> ModuleKey
      testSplice (ModuleInfo {_moduleKey = key@(ModuleKey {_moduleName = A.ModuleName () "SrcLoc"})}) exp' =
          case unfoldApply exp' of
            (x : _) | hasName x "makeLenses" || hasName x "makeLensesFor" -> key {_moduleName = A.ModuleName () "Scan"}
            _ -> key
      testSplice i _ = _moduleKey i
      hasName x s = elem (A.Ident () s) (map simplify (gFind x :: [A.Name ()]))
      unfoldApply (A.App _ a b) = unfoldApply a ++ [b]
      unfoldApply x = [x]
      instPred (ModuleInfo {_moduleKey = key@(ModuleKey {_moduleName = A.ModuleName () "SrcLoc"})}) name _types
          | (gFind name :: [A.Name ()]) == [A.Ident () "SpanInfo"] =
              key {_moduleName = A.ModuleName () "Scan"}
          | (gFind name :: [A.Name ()]) == [A.Ident () "EndLoc"] =
              key {_moduleName = A.ModuleName () "Scan"}
      instPred i _ _ = _moduleKey i

-- Because Imports imports Symbols, Symbols is reachable from Imports.
-- Moving a declaration from Symbols to Imports is an Up move.
decl8 :: Test
decl8 = TestLabel "decl8 - up move" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testMoveSpec' "tests/expected/decl8" input (runSimpleMoveUnsafe input spec)
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [-- moveDeclsByName "defaultCpphsOptions" "CPP" "Types"
                           -- moveDeclsByName "parseFileWithCommentsAndCPP" "CPP" "Types"
                            moveDeclsByName "symbolsDeclaredBy" "Symbols" "Imports"
                          , moveDeclsByName "imports" "Symbols" "Imports"
                          , moveDeclsByName "exports" "Symbols" "Imports"
                          ]

-- Need a way to add imports of the lenses created by makeLenses
decl9 :: Test
decl9 = TestLabel "decl9" $ TestCase $ testMoveSpec' "tests/expected/decl9" "tests/input/rgh" $
          runMoveUnsafe "tests/input/rgh" [".", "tests"] spec
    where
      spec = foldl1' (<>) [moveDeclsByName "MoveType" "Decls" "Graph",
                           moveDeclsByName "findModuleByKey" "Decls" "Graph",
                           moveDeclsByName "moveType" "Decls" "Graph",
                           moveDeclsByName "moveType'" "Decls" "Graph",
                           moveDeclsByName "importsSymbolsFrom" "Decls" "Graph",
                           moveDeclsByName "Rd" "Decls" "Graph"]

load8 :: Test
load8 = TestLabel "load8" $ TestCase $
          withCurrentDirectory "/home/dsf/git/happstack-ghcjs/happstack-ghcjs-client" $ do
          -- withCleanRepo $ do
            let opts = GHCOpts { hc = "ghcjs"
                               , hsSourceDirs=["client", "../happstack-ghcjs-webmodule"]
                               , cppOptions = defaultCpphsOptions
                               , enabled = [CPP, OverloadedStrings, ExtendedDefaultRules]
                               , hashDefines = [] }
            let optSets = [ opts {hc = "ghc",   hashDefines = map (fromJust . parseHashDefine False) [["define", "CLIENT", "0"], ["define", "SERVER", "1"], ["undef", "NO_TH"], ["define", "SERVE_DYNAMIC"]]}
                          , opts {hc = "ghcjs", hashDefines = map (fromJust . parseHashDefine False) [["define", "CLIENT", "1"], ["define", "SERVER", "0"], ["define", "NO_TH"], ["define", "SERVE_DYNAMIC"]]}
                          , opts {hc = "ghc",   hashDefines = map (fromJust . parseHashDefine False) [["define", "CLIENT", "0"], ["define", "SERVER", "1"], ["undef", "NO_TH"], ["undef", "SERVE_DYNAMIC"]]}
                          , opts {hc = "ghcjs", hashDefines = map (fromJust . parseHashDefine False) [["define", "CLIENT", "1"], ["define", "SERVER", "0"], ["define", "NO_TH"], ["undef", "SERVE_DYNAMIC"]]} ]
            m <- loadModule opts "client/Examples/MVExample.hs"
            cleanImports optSets [m]
            (code, diff, err) <- readProcessWithExitCode "diff" ["-ruN", expected, actual] ""
            case code of
              ExitSuccess -> assertString diff
              ExitFailure 1 -> assertString diff
              ExitFailure 2 -> assertString err
              ExitFailure n -> error $ "Unexpected diff(1) exit code: " ++ show n
    where
      expected = "/home/dsf/git/refact-global-hse/tests/expected/decl8"
      actual = "/home/dsf/git/happstack-ghcjs/happstack-ghcjs-client"
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "foo" "Bar" "Baz"]

load9 :: Test
load9 = TestLabel "load9" $ TestCase $
          withCurrentDirectory "/home/dsf/git/happstack-ghcjs/happstack-ghcjs-client" $
          withCleanRepo $ do
            let opts = GHCOpts {hc = "ghc",
                                hsSourceDirs=["client", "../happstack-ghcjs-webmodule"],
                                cppOptions = defaultCpphsOptions,
                                enabled = [CPP, OverloadedStrings, ExtendedDefaultRules],
                                hashDefines = map (fromJust . parseHashDefine False) [["define", "CLIENT", "0"], ["define", "SERVER", "1"], ["undef", "NO_TH"], ["define", "SERVE_DYNAMIC"]]}
            m <- loadModule opts"client/Examples/MVExample.hs"
            cleanImports [opts] [m]
            (code, diff, err) <- readProcessWithExitCode "diff" ["-ruN", expected, actual] ""
            case code of
              ExitSuccess -> assertString diff
              ExitFailure 1 -> assertString diff
              ExitFailure 2 -> assertString err
    where
      expected = "/home/dsf/git/refact-global-hse/tests/expected/decl8"
      actual = "/home/dsf/git/happstack-ghcjs/happstack-ghcjs-client"
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "foo" "Bar" "Baz"]

simple1 :: Test
simple1 =
     TestLabel "simple1" $ TestCase $
      testMoveSpec' "tests/expected/simple1" "tests/input/simple" $
        runSimpleMoveUnsafe "tests/input/simple" (moveDeclsByName "listPairs" "A" "B" :: MoveSpec)

simple2 :: Test
simple2 =
     TestLabel "simple2" $ TestCase $
      testMoveSpec' "tests/expected/simple2" "tests/input/simple2" $
        runSimpleMoveUnsafe "tests/input/simple2" (moveDeclsByName "MoveType" "C" "D" :: MoveSpec)

simple3 :: Test
simple3 =
     TestLabel "simple3" $ TestCase $
      testMoveSpec' "tests/expected/simple3" "tests/input/simple3" $
        runSimpleMoveUnsafe "tests/input/simple3" (moveDeclsByName "MoveType" "C" "D" :: MoveSpec)

-- Perform the same move with these CPP flag combinations:
--
--    -DSERVER=1 -DCLIENT=0
--    -DSERVER=1 -DCLIENT=0 -DSERVE_DYNAMIC
--    -DSERVER=0 -DCLIENT=1
--    -DSERVER=0 -DCLIENT=1 -DSERVE_DYNAMIC
--
-- then somehow merge the different results.  The issue I can forsee
-- is if differing changes are produced for code that is visible from
-- more than one of the listed combinations.  In that case new ifdefs
-- must be generated.
decl10 :: Test
decl10 =
    TestLabel "decl10" $ TestCase $
    testMoveSpec' "tests/expected/decl10" "tests/input/decl10" $
    runSimpleMoveUnsafe "tests/input/simple3" undefined

testMoveSpec :: FilePath -> FilePath -> MoveSpec -> IO ()
testMoveSpec expected actual moveSpec =
    testMoveSpec' expected actual $ runSimpleMoveUnsafe actual moveSpec

testMoveSpec' :: FilePath -> FilePath -> IO () -> IO ()
testMoveSpec' expected actual action = do
  gitResetSubdir actual
  action
  (code, diff, _) <- readProcessWithExitCode "diff" ["-ruN", expected, actual] ""
  when (code == ExitSuccess) (gitResetSubdir actual)
  assertString diff

testSpec :: forall l. (SrcInfo l, Data l) => MoveSpec -> ModuleInfo l -> IO (ModuleInfo l)
testSpec moveSpec i@(ModuleInfo {_moduleKey = k, _module = A.Module _ _ _ _ ds}) = do
  putStrLn ("---- module " ++ show (moduleName k) ++ " ----")
  mapM_ (\d -> let k' = applyMoveSpec moveSpec i d in
               putStrLn (ezPrint (i, d) ++ ": " ++ if k /= k' then show k ++ " " ++  " -> " ++ show k' else "unchanged")) ds
  return i
testSpec _ _ = error "Unexpected module"
