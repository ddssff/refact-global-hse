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

import Clean (cleanImports)
import Control.Lens (set, over)
import Control.Monad (when)
import CPP (cabalMacro, cppOptions, defaultCpphsOptions, enabled, GHCOpts, ghcOptions, hashDefines, hc, hsSourceDirs)
import Data.Data (Data)
import Data.Default (def)
import Data.List hiding (find)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Version (Version(Version))
import Decls (runMoveUnsafe)
import HashDefine (parseHashDefine)
import Language.Haskell.Exts (Decl(FunBind, TypeSig), Match(InfixMatch, Match),
   ModuleName(ModuleName), Name(Ident), Exp(App),
   KnownExtension(CPP, OverloadedStrings, ExtendedDefaultRules), Module(Module), SrcInfo)
import Language.Haskell.Names.SyntaxUtils (dropAnn)
import Language.Preprocessor.Cpphs (parseOptions)
import LoadModule (loadModule)
import ModuleInfo (ModuleInfo(ModuleInfo, _module, _moduleKey))
import ModuleKey (ModuleKey(ModuleKey, _moduleName), moduleName)
import MoveSpec (applyMoveSpec, moveDeclsByName, moveInstDecls, MoveSpec(MoveSpec), moveSpliceDecls)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertString, Test(..))
import Utils (EZPrint(ezPrint), gFind, gitResetSubdir, withCleanRepo, withCurrentDirectory)

declTests :: Test
declTests = TestList [decl1, decl2, decl3, simple4, simple5, decl6, {-decl7,-} decl8, {-decl9,-}
                      clean10,
                      simple1, simple2, simple3]

-- Test moving a declaration to a module that currently imports it
decl1 :: Test
decl1 = TestLabel "decl1 - move tryFindM from Lib to Tableaux" $ TestCase $ testMoveSpec "tests/expected/decl1" "tests/input/atp-haskell" moveSpec1

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
      f i (TypeSig _ [Ident _ s] _)
          | s == "tryfindM" {-|| s == "failing"-} =
              (_moduleKey i) {_moduleName = ModuleName () "Data.Logic.ATP.Tableaux"}
      f i (FunBind _ ms)
          | any (`elem` [Ident () "tryfindM" {-, S.Ident () "failing"-}])
                (map (\match -> case match of
                                  Match _ name _ _ _ -> name
                                  InfixMatch _ _ name _ _ _ -> name) ms) =
              (_moduleKey i) {_moduleName = ModuleName () "Data.Logic.ATP.Tableaux"}
      f i __ = _moduleKey i
{-
moveSpec1 k d | Set.member (S.Ident "tryfindM") (foldDeclared Set.insert mempty d) =
                  trace ("Expected TypeSig or FunBind: " ++ show d)
                        (k {_modulePath = "Data/Logic/ATP/Tableaux.hs",
                            _moduleName = S.ModuleName "Data.Logic.ATP.Tableaux"})
moveSpec1 k d@(FunBind _ _) = {-trace ("FunBind: " ++ show (foldDeclared (:) mempty d))-} k
moveSpec1 k (ClassDecl _ _mcxt _dh _fd _mcds) = k
moveSpec1 k (DataDecl _ _dn _mcxt _dh _qcs _md) = k
moveSpec1 k (PatBind _ _p _rhs _mbs) = k
moveSpec1 k (TypeDecl _ _dh _typ) = k
moveSpec1 k (TypeSig _ _name _typ) = k
moveSpec1 k (InstDecl _ _mo _ir _mids) = k
moveSpec1 k (InfixDecl _ _assoc _mi _ops) = k
moveSpec1 k (DerivDecl {}) = k
moveSpec1 k d = error $ "Unexpected decl: " ++ take 120 (show d) ++ ".."
-}

-- Test moving a declaration to an as yet non-existant module
-- Test updating import of decl that moved from A to B in module C
decl2 :: Test
decl2 = TestLabel "decl2" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testDirectory "tests/expected/decl2" input
            (runMoveUnsafe input opts0 (moveDeclsByName "withCurrentDirectory" "Utils" "Tmp" :: MoveSpec))

-- Test moving a declaration to a module that does *not* currently
-- import it.  Now we don't know whether it leaves behind uses for
-- which we need to import it (which might be called "moving down") or
-- if we are moving it to the place where it is used (which could be
-- called "moving up".)  (Not sure whether this test still fits this
-- description.)
decl3 :: Test
decl3 = TestLabel "decl3" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testDirectory "tests/expected/decl3" input
            (runMoveUnsafe input opts0 (moveDeclsByName "lines'" "Utils" "Tmp" :: MoveSpec) >>
             runMoveUnsafe input opts0 (moveDeclsByName "lines'" "Tmp" "SrcLoc" :: MoveSpec))

simple4 :: Test
simple4 = TestLabel "simple4" $ TestCase $ do
            let input = "tests/input/simple4"
            testDirectory "tests/expected/simple4" input (runMoveUnsafe input opts0 spec)
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "s2" "M1" "M2"]

-- | Like simple4, but there's an #if around s2
simple5 :: Test
simple5 = TestLabel "simple5" $ TestCase $ do
            let input = "tests/input/simple5"
                Right cpp = parseOptions ["-DMIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)"]
                opts = set cppOptions cpp opts0
            testDirectory "tests/expected/simple5" input (runMoveUnsafe input opts spec)
    where
      spec :: MoveSpec
      spec = foldl1' (<>) [moveDeclsByName "s2" "M1" "M2"]

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
decl7 = TestLabel "decl7" $ TestCase $ testDirectory "tests/expected/decl7" "tests/input/rgh" $
          runMoveUnsafe "tests/input/rgh" (set hsSourceDirs [".", "tests"] opts0) spec
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
      testSplice :: ModuleInfo () -> Exp () -> ModuleKey
      testSplice (ModuleInfo {_moduleKey = key@(ModuleKey {_moduleName = ModuleName () "SrcLoc"})}) exp' =
          case unfoldApply exp' of
            (x : _) | hasName x "makeLenses" || hasName x "makeLensesFor" -> key {_moduleName = ModuleName () "Scan"}
            _ -> key
      testSplice i _ = _moduleKey i
      hasName x s = elem (Ident () s) (map dropAnn (gFind x :: [Name ()]))
      unfoldApply (App _ a b) = unfoldApply a ++ [b]
      unfoldApply x = [x]
      instPred (ModuleInfo {_moduleKey = key@(ModuleKey {_moduleName = ModuleName () "SrcLoc"})}) name _types
          | (gFind name :: [Name ()]) == [Ident () "SpanInfo"] =
              key {_moduleName = ModuleName () "Scan"}
          | (gFind name :: [Name ()]) == [Ident () "EndLoc"] =
              key {_moduleName = ModuleName () "Scan"}
      instPred i _ _ = _moduleKey i

-- Because Imports imports Symbols, Symbols is reachable from Imports.
-- Moving a declaration from Symbols to Imports is an Up move.
decl8 :: Test
decl8 = TestLabel "decl8 - up move" $ TestCase $ do
          let input = "tests/input/decl-mover"
          testDirectory "tests/expected/decl8" input (runMoveUnsafe input opts0 spec)
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
decl9 = TestLabel "decl9" $ TestCase $ testDirectory "tests/expected/decl9" "tests/input/rgh" $
          runMoveUnsafe "tests/input/rgh" (set hsSourceDirs [".", "tests"] opts0) spec
    where
      spec = foldl1' (<>) [moveDeclsByName "MoveType" "Decls" "Graph",
                           moveDeclsByName "findModuleByKey" "Decls" "Graph",
                           moveDeclsByName "moveType" "Decls" "Graph",
                           moveDeclsByName "moveType'" "Decls" "Graph",
                           moveDeclsByName "importsSymbolsFrom" "Decls" "Graph",
                           moveDeclsByName "Rd" "Decls" "Graph"]

clean8 :: Test
clean8 = TestLabel "load8" $ TestCase $
          withCurrentDirectory "/home/dsf/git/happstack-ghcjs/happstack-ghcjs-client" $ do
          -- withCleanRepo $ do
            let opts = set hc "ghcjs" $
                       set hsSourceDirs ["client", "../happstack-ghcjs-webmodule"] $
                       set cppOptions defaultCpphsOptions $
                       set enabled [CPP, OverloadedStrings, ExtendedDefaultRules] $
                       opts0
            let optSets = [ set hc "ghc" (set hashDefines (map (fromJust . parseHashDefine False) [["define", "CLIENT", "0"], ["define", "SERVER", "1"], ["undef", "NO_TH"], ["define", "SERVE_DYNAMIC"]]) opts)
                          , set hc "ghcjs" (set hashDefines (map (fromJust . parseHashDefine False) [["define", "CLIENT", "1"], ["define", "SERVER", "0"], ["define", "NO_TH"], ["define", "SERVE_DYNAMIC"]]) opts)
                          , set hc "ghc" (set hashDefines (map (fromJust . parseHashDefine False) [["define", "CLIENT", "0"], ["define", "SERVER", "1"], ["undef", "NO_TH"], ["undef", "SERVE_DYNAMIC"]]) opts)
                          , set hc "ghcjs" (set hashDefines (map (fromJust . parseHashDefine False) [["define", "CLIENT", "1"], ["define", "SERVER", "0"], ["define", "NO_TH"], ["undef", "SERVE_DYNAMIC"]]) opts) ]
            m <- loadModule opts (Just "client", "Examples/MVExample.hs")
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

clean9 :: Test
clean9 = TestLabel "load9" $ TestCase $
          withCurrentDirectory "/home/dsf/git/happstack-ghcjs/happstack-ghcjs-client" $
          withCleanRepo $ do
            let opts = set hc "ghc" $
                       set hsSourceDirs ["client", "../happstack-ghcjs-webmodule"] $
                       set cppOptions defaultCpphsOptions $
                       set enabled [CPP, OverloadedStrings, ExtendedDefaultRules] $
                       set hashDefines (map (fromJust . parseHashDefine False) [["define", "CLIENT", "0"], ["define", "SERVER", "1"], ["undef", "NO_TH"], ["define", "SERVE_DYNAMIC"]]) $
                       opts0
            m <- loadModule opts (Just "client", "Examples/MVExample.hs")
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

clean10 :: Test
clean10 =
    TestLabel "clean10" $ TestCase $ do
      gitResetSubdir "tests/input/atp-haskell/Data/Logic/ATP"
      withCurrentDirectory "tests/input/atp-haskell" $ do
        let opts = set hc "ghc" $
                   set hsSourceDirs [] $
                   set cppOptions defaultCpphsOptions $
                   set enabled [] $
                   opts0
        m <- loadModule opts (Nothing, "Data/Logic/ATP/Lib.hs")
        cleanImports [opts] [m]
      (code, diff, err) <- readProcessWithExitCode "diff" ["-ruN", expected, actual] ""
      case code of
        ExitSuccess -> assertString diff -- No differences
        ExitFailure 1 -> assertString diff -- Differences, no error
        ExitFailure 2 -> assertString err -- error
    where
      expected = "tests/expected/clean10/Lib.hs"
      actual = "tests/input/atp-haskell/Data/Logic/ATP/Lib.hs"

simple1 :: Test
simple1 =
     TestLabel "simple1" $ TestCase $
      testDirectory "tests/expected/simple1" "tests/input/simple" $
        runMoveUnsafe "tests/input/simple" opts0 (moveDeclsByName "listPairs" "A" "B" :: MoveSpec)

simple2 :: Test
simple2 =
     TestLabel "simple2" $ TestCase $
      testDirectory "tests/expected/simple2" "tests/input/simple2" $
        runMoveUnsafe "tests/input/simple2" opts0 (moveDeclsByName "MoveType" "C" "D" :: MoveSpec)

simple3 :: Test
simple3 =
     TestLabel "simple3" $ TestCase $
      testDirectory "tests/expected/simple3" "tests/input/simple3" $
        runMoveUnsafe "tests/input/simple3" opts0 (moveDeclsByName "MoveType" "C" "D" :: MoveSpec)

opts0 :: GHCOpts
opts0 =
    over hashDefines (++ [cabalMacro "base" (Version [4,8] [])]) o
    where o = set hsSourceDirs ["."] def

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
    testDirectory "tests/expected/decl10" "tests/input/decl10" $
    runMoveUnsafe "tests/input/simple3" opts0 undefined

-- | Run a move spec on contents of directory actual and compare to
-- contents of directory expected.
testMoveSpec :: FilePath -> FilePath -> MoveSpec -> IO ()
testMoveSpec expected actual moveSpec =
    testDirectory expected actual $ runMoveUnsafe actual opts0 moveSpec

-- | Perform an IO action on directory actual and compare to the
-- contents of directory expected.
testDirectory :: FilePath -> FilePath -> IO () -> IO ()
testDirectory expected actual action = do
  gitResetSubdir actual
  action
  (code, diff, _) <- readProcessWithExitCode "diff" ["-ruN", expected, actual] ""
  when (code == ExitSuccess) (gitResetSubdir actual)
  assertString diff

testSpec :: forall l. (SrcInfo l, Data l) => MoveSpec -> ModuleInfo l -> IO (ModuleInfo l)
testSpec moveSpec i@(ModuleInfo {_moduleKey = k, _module = Module _ _ _ _ ds}) = do
  putStrLn ("---- module " ++ show (moduleName k) ++ " ----")
  mapM_ (\d -> let k' = applyMoveSpec moveSpec i d in
               putStrLn (ezPrint (i, d) ++ ": " ++ if k /= k' then show k ++ " " ++  " -> " ++ show k' else "unchanged")) ds
  return i
testSpec _ _ = error "Unexpected module"
