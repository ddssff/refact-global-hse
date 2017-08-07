{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Generics (everywhere, mkT)
import Data.List (intercalate)
import Data.Map.Strict as Map (fromList)
import DeclTests
import ImportTests
import Language.Haskell.Exts -- (parseModule, ParseResult(ParseOk))
import Language.Haskell.Exts.CPP (parseFileWithCommentsAndCPP)
--import Language.Haskell.Exts.Syntax
import Language.Haskell.Names
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Haskell.Names.Imports (importTable)
import Refactor.CPP (cppOptions, defaultParseMode, tests, turnOffLocations)
import Refactor.LoadModule (loadModule)
import Refactor.ModuleInfo
import Refactor.ModuleKey (ModuleKey(..))
import Refactor.ScanM (debugRender)
import Refactor.SrcLoc
import Refactor.Utils
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (assertEqual, errors, failures, runTestTT, Test(TestCase, TestList))

-- Note that running one of these tests by itself will leave the input
-- subdirectory in a modified state.  This allows you to look at what
-- actually happened, that is, what you saw in the error diff.  These
-- changes are reset before the next test begins.

main :: IO ()
main = runTestTT (TestList [Refactor.CPP.tests, importTests, declTests, cpp1]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)

cpp1 :: Test
cpp1 = TestCase $ do
         (ModuleInfo {_module = m, _moduleText = s, _moduleComments = cs}) <- loadModule def (Just "tests/input/cpp", "A.hs")
         assertEqual "cpp1" expected (debugRender m cs s)
    where
      expected =
          intercalate "\n" ["[[{-# LANGUAGE CPP #-}]",
                            "[module A where]",
                            "",
                            "[import Data.String]",
                            "",
                            "#if 0",
                            "x = \"This is inside #if 0\"",
                            "#else",
                            "[x = \"This is inside the #else\"]",
                            "#endif",
                            "",
                            "[y = \"This is outside the #if\"]",
                            "]"]

cpp2 :: Test
cpp2 =
    TestList
    [ -- We can't determine what #if conditions affect which import
      -- from what we get back from loadModule.  Must go deeper.
      TestCase $ do
        input <- readFile "tests/input/If.hs"
        m <- loadModule def (Nothing, "tests/input/If.hs")
        assertEqual "cpp2"
           (Module () Nothing [] [] [])
           (fmap (const ()) (_module m))
    , TestCase $ do
        let path = "tests/input/If.hs"
            opts' = def -- foldr applyHashDefine opts (view hashDefines opts)
            opts'' = over cppOptions turnOffLocations opts'
            mode = Refactor.CPP.defaultParseMode opts'' path
        moduleText <- liftIO $ readFile path
        ParseOk (parsed', comments) <- parseFileWithCommentsAndCPP (view cppOptions opts'') mode path
        let parsed = mapTopAnnotations (fixEnds comments moduleText) $ everywhere (mkT fixSpan) parsed'
        assertEqual "cpp3"
           (_module mi, [], _module mi)
           (parsed', comments, parsed)
    ]

mi :: ModuleInfo SrcSpanInfo
mi =
    ModuleInfo
    {_moduleKey = ModuleKey {_moduleTop = "/home/dsf/git/refact-global-hse/tests/input", _moduleName = ModuleName () "If", _moduleExt = ".hs"},
     _module = Module (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 7 1 29 1, srcInfoPoints = [SrcSpan "tests/input/If.hs" 7 1 7 1,SrcSpan "tests/input/If.hs" 9 1 9 1,SrcSpan "tests/input/If.hs" 9 1 9 1,SrcSpan "tests/input/If.hs" 12 1 12 1,SrcSpan "tests/input/If.hs" 13 1 13 1,SrcSpan "tests/input/If.hs" 18 1 18 1,SrcSpan "tests/input/If.hs" 24 1 24 1,SrcSpan "tests/input/If.hs" 29 1 29 1,SrcSpan "tests/input/If.hs" 29 1 29 1]})
                (Just (ModuleHead (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 9 1 9 16, srcInfoPoints = [SrcSpan "tests/input/If.hs" 9 1 9 7,SrcSpan "tests/input/If.hs" 9 11 9 16]})
                         (ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 9 8 9 10, srcInfoPoints = []}) "If") Nothing Nothing))
                [LanguagePragma (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 7 1 7 59, srcInfoPoints = [SrcSpan "tests/input/If.hs" 7 1 7 13,SrcSpan "tests/input/If.hs" 7 17 7 18,SrcSpan "tests/input/If.hs" 7 36 7 37,SrcSpan "tests/input/If.hs" 7 56 7 59]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 7 14 7 17, srcInfoPoints = []}) "CPP",Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 7 19 7 36, srcInfoPoints = []}) "FlexibleInstances",Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 7 38 7 55, srcInfoPoints = []}) "OverloadedStrings"]]
                [ImportDecl {importAnn = SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 1 12 38, srcInfoPoints = [SrcSpan "tests/input/If.hs" 12 1 12 7]}, importModule = ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 8 12 17, srcInfoPoints = []}) "Data.Text", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (ImportSpecList (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 18 12 38, srcInfoPoints = [SrcSpan "tests/input/If.hs" 12 18 12 19,SrcSpan "tests/input/If.hs" 12 22 12 23,SrcSpan "tests/input/If.hs" 12 30 12 31,SrcSpan "tests/input/If.hs" 12 37 12 38]}) False [IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 19 12 22, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 19 12 22, srcInfoPoints = []}) "all"),IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 24 12 30, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 24 12 30, srcInfoPoints = []}) "unpack"),IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 32 12 37, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 12 32 12 37, srcInfoPoints = []}) "empty")])},
                 ImportDecl {importAnn = SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 1 13 42, srcInfoPoints = [SrcSpan "tests/input/If.hs" 13 1 13 7]}, importModule = ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 8 13 15, srcInfoPoints = []}) "Prelude", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (ImportSpecList (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 16 13 42, srcInfoPoints = [SrcSpan "tests/input/If.hs" 13 16 13 22,SrcSpan "tests/input/If.hs" 13 23 13 24,SrcSpan "tests/input/If.hs" 13 27 13 28,SrcSpan "tests/input/If.hs" 13 34 13 35,SrcSpan "tests/input/If.hs" 13 41 13 42]}) True [IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 24 13 27, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 24 13 27, srcInfoPoints = []}) "all"),IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 29 13 34, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 29 13 34, srcInfoPoints = []}) "break"),IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 36 13 41, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 13 36 13 41, srcInfoPoints = []}) "empty")])},
                 -- This import is inside #if !__GHCJS__
                 ImportDecl {importAnn = SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 1 18 31, srcInfoPoints = [SrcSpan "tests/input/If.hs" 18 1 18 7]}, importModule = ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 8 18 17, srcInfoPoints = []}) "Data.Text", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (ImportSpecList (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 18 18 31, srcInfoPoints = [SrcSpan "tests/input/If.hs" 18 18 18 19,SrcSpan "tests/input/If.hs" 18 24 18 25,SrcSpan "tests/input/If.hs" 18 30 18 31]}) False [IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 19 18 24, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 19 18 24, srcInfoPoints = []}) "break"),IVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 26 18 30, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 18 26 18 30, srcInfoPoints = []}) "drop")])}]
               [PatBind (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 24 1 28 43, srcInfoPoints = []}) (PVar (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 24 1 24 5, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 24 1 24 5, srcInfoPoints = []}) "main")) (UnGuardedRhs (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 24 6 28 43, srcInfoPoints = [SrcSpan "tests/input/If.hs" 24 6 24 7]}) (Do (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 24 8 28 43, srcInfoPoints = [SrcSpan "tests/input/If.hs" 24 8 24 10,SrcSpan "tests/input/If.hs" 26 3 26 3,SrcSpan "tests/input/If.hs" 28 3 28 3,SrcSpan "tests/input/If.hs" 29 1 29 0]}) [Qualifier (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 3 26 53, srcInfoPoints = []}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 3 26 53, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 3 26 11, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 3 26 11, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 3 26 11, srcInfoPoints = []}) "putStrLn"))) (Paren (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 12 26 53, srcInfoPoints = [SrcSpan "tests/input/If.hs" 26 12 26 13,SrcSpan "tests/input/If.hs" 26 52 26 53]}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 13 26 52, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 13 26 19, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 13 26 19, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 13 26 19, srcInfoPoints = []}) "unpack"))) (Paren (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 20 26 52, srcInfoPoints = [SrcSpan "tests/input/If.hs" 26 20 26 21,SrcSpan "tests/input/If.hs" 26 51 26 52]}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 21 26 51, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 21 26 24, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 21 26 24, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 21 26 24, srcInfoPoints = []}) "snd"))) (Paren (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 25 26 51, srcInfoPoints = [SrcSpan "tests/input/If.hs" 26 25 26 26,SrcSpan "tests/input/If.hs" 26 50 26 51]}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 26 26 50, srcInfoPoints = []}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 26 26 40, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 26 26 31, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 26 26 31, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 26 26 31, srcInfoPoints = []}) "break"))) (RightSection (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 32 26 40, srcInfoPoints = [SrcSpan "tests/input/If.hs" 26 32 26 33,SrcSpan "tests/input/If.hs" 26 39 26 40]}) (QVarOp (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 33 26 35, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 33 26 35, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 33 26 35, srcInfoPoints = []}) "=="))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 36 26 39, srcInfoPoints = []}) (Char (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 36 26 39, srcInfoPoints = []}) 'a' "a")))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 41 26 50, srcInfoPoints = []}) (String (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 26 41 26 50, srcInfoPoints = []}) "abcbabc" "abcbabc"))))))))),Qualifier (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 3 28 43, srcInfoPoints = []}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 3 28 43, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 3 28 11, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 3 28 11, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 3 28 11, srcInfoPoints = []}) "putStrLn"))) (Paren (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 12 28 43, srcInfoPoints = [SrcSpan "tests/input/If.hs" 28 12 28 13,SrcSpan "tests/input/If.hs" 28 42 28 43]}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 13 28 42, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 13 28 17, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 13 28 17, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 13 28 17, srcInfoPoints = []}) "show"))) (Paren (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 18 28 42, srcInfoPoints = [SrcSpan "tests/input/If.hs" 28 18 28 19,SrcSpan "tests/input/If.hs" 28 41 28 42]}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 19 28 41, srcInfoPoints = []}) (App (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 19 28 31, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 19 28 22, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 19 28 22, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 19 28 22, srcInfoPoints = []}) "all"))) (RightSection (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 23 28 31, srcInfoPoints = [SrcSpan "tests/input/If.hs" 28 23 28 24,SrcSpan "tests/input/If.hs" 28 30 28 31]}) (QVarOp (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 24 28 26, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 24 28 26, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 24 28 26, srcInfoPoints = []}) "=="))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 27 28 30, srcInfoPoints = []}) (Char (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 27 28 30, srcInfoPoints = []}) 'a' "a")))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 32 28 41, srcInfoPoints = []}) (String (SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 28 32 28 41, srcInfoPoints = []}) "abcbabc" "abcbabc")))))))])) Nothing],
     _moduleComments = [Comment False (SrcSpan "tests/input/If.hs" 1 1 1 68) " | A module with some imports protected by #if.  When the cleaner",
                        Comment False (SrcSpan "tests/input/If.hs" 2 1 2 68) " runs it can only understand one set of CPP variables.  It should",
                        Comment False (SrcSpan "tests/input/If.hs" 3 1 3 68) " not modify imports which it cannot see.  It should clean imports",
                        Comment False (SrcSpan "tests/input/If.hs" 4 1 4 72) " that are inside a combination of #ifs where it can see, leaving them",
                        Comment False (SrcSpan "tests/input/If.hs" 5 1 5 40) " inside the #ifs where they appeared.",
                        Comment False (SrcSpan "tests/input/If.hs" 11 1 11 73) " Imports always visible - should be cleaned.  Empty should be removed.",
                        -- There is a #if !__GHCJS__ here
                        Comment False (SrcSpan "tests/input/If.hs" 16 1 16 72) " imports visible to GHC - should be cleaned and kept inside an ifdef.",
                        Comment False (SrcSpan "tests/input/If.hs" 17 1 17 49) " Break should be kept, drop should be removed."
                        -- The comment in the #else is already gone.  It is at line 20
                       ],
     _modulePath = "tests/input/If.hs",
     _moduleText = "-- | A module with some imports protected by #if.  When the cleaner\n-- runs it can only understand one set of CPP variables.  It should\n-- not modify imports which it cannot see.  It should clean imports\n-- that are inside a combination of #ifs where it can see, leaving them\n-- inside the #ifs where they appeared.\n\n{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings #-}\n\nmodule If where\n\n-- Imports always visible - should be cleaned.  Empty should be removed.\nimport Data.Text (all, unpack, empty)\nimport Prelude hiding (all, break, empty)\n\n#if !__GHCJS__\n-- imports visible to GHC - should be cleaned and kept inside an ifdef.\n-- Break should be kept, drop should be removed.\nimport Data.Text (break, drop)\n#else\n-- Imports not visible to GHC - should be left alone even though not used.\nimport Data.Text (count)\n#endif\n\nmain = do\n#if !__GHCJS__\n  putStrLn (unpack (snd (break (== 'a') \"abcbabc\")))\n#endif\n  putStrLn (show (all (== 'a') \"abcbabc\"))\n",
     _moduleSpan = SrcSpanInfo {srcInfoSpan = SrcSpan "tests/input/If.hs" 1 1 29 1, srcInfoPoints = []}, _moduleGlobals = fromList []}

names1 :: Test
names1 = TestCase $ assertEqual "names1" expected actual
    where
      actual = parsemod "module M   (f)   where\nf x | x > 0 = x + 1\n"
      expected = Module
                   (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 3 1, srcInfoPoints = [SrcSpan "<unknown>.hs" 1 1 1 1,SrcSpan "<unknown>.hs" 1 1 1 1,SrcSpan "<unknown>.hs" 2 1 2 1,SrcSpan "<unknown>.hs" 3 1 3 1,SrcSpan "<unknown>.hs" 3 1 3 1]}))
                   (Just (ModuleHead
                          (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 1 23, srcInfoPoints = [SrcSpan "<unknown>.hs" 1 1 1 7,SrcSpan "<unknown>.hs" 1 18 1 23]}))
                          (ModuleName (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 8 1 9, srcInfoPoints = []})) "M")
                          Nothing
                          (Just (ExportSpecList
                                 (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 12 1 15, srcInfoPoints = [SrcSpan "<unknown>.hs" 1 12 1 13,SrcSpan "<unknown>.hs" 1 14 1 15]}))
                                 [EVar
                                  (Scoped (Export [Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}]) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 13 1 14, srcInfoPoints = []}))
                                  (UnQual
                                   (Scoped (GlobalSymbol (Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}) (UnQual () (Ident () "f"))) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 13 1 14, srcInfoPoints = []}))
                                   (Ident (Scoped (GlobalSymbol (Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}) (UnQual () (Ident () "f"))) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 13 1 14, srcInfoPoints = []})) "f"))]))))
                   []
                   []
                   [FunBind
                    (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 1 2 20, srcInfoPoints = []}))
                    [Match
                     (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 1 2 20, srcInfoPoints = []}))
                     (Ident (Scoped ValueBinder (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 1 2 2, srcInfoPoints = []})) "f")
                     [PVar (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 3 2 4, srcInfoPoints = []})) (Ident (Scoped ValueBinder (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 3 2 4, srcInfoPoints = []})) "x")]
                     (GuardedRhss
                      (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 5 2 20, srcInfoPoints = [SrcSpan "<unknown>.hs" 2 5 2 6,SrcSpan "<unknown>.hs" 2 13 2 14]}))
                      [GuardedRhs
                       (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 5 2 20, srcInfoPoints = [SrcSpan "<unknown>.hs" 2 5 2 6,SrcSpan "<unknown>.hs" 2 13 2 14]}))
                       [Qualifier
                        (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 7 2 12, srcInfoPoints = []}))
                        (InfixApp
                         (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 7 2 12, srcInfoPoints = []}))
                         (Var (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 7 2 8, srcInfoPoints = []})) (UnQual (Scoped (LocalValue (SrcLoc "<unknown>.hs" 2 3)) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 7 2 8, srcInfoPoints = []})) (Ident (Scoped (LocalValue (SrcLoc "<unknown>.hs" 2 3)) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 7 2 8, srcInfoPoints = []})) "x")))
                         (QVarOp (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []})) (UnQual (Scoped (ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []}) ">")))) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []})) (Symbol (Scoped (ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []}) ">")))) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []})) ">")))
                         (Lit (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 11 2 12, srcInfoPoints = []})) (Int (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 11 2 12, srcInfoPoints = []})) 0 "0")))]
                       (InfixApp
                        (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 15 2 20, srcInfoPoints = []}))
                        (Var (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 15 2 16, srcInfoPoints = []})) (UnQual (Scoped (LocalValue (SrcLoc "<unknown>.hs" 2 3)) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 15 2 16, srcInfoPoints = []})) (Ident (Scoped (LocalValue (SrcLoc "<unknown>.hs" 2 3)) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 15 2 16, srcInfoPoints = []})) "x")))
                        (QVarOp (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []})) (UnQual (Scoped (ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []}) "+")))) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []})) (Symbol (Scoped (ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []}) "+")))) (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []})) "+")))
                        (Lit (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 19 2 20, srcInfoPoints = []})) (Int (Scoped None (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 19 2 20, srcInfoPoints = []})) 1 "1")))])
                     Nothing]]
      parsemod s = let ParseOk m = parseModule s in let env = resolve [m] mempty in annotate env m

names2 :: Test
names2 = TestCase $ do
           assertEqual "names2" expected actual
    where
      actual = gFind (parsemod "module M   (f)   where\nf x | x > 0 = x + 1\n") :: [NameInfo SrcSpanInfo]
      expected = [None,None,None,None,
#if 1
                  Export [Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}],
                  GlobalSymbol (Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}) (UnQual () (Ident () "f")),
                  GlobalSymbol (Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}) (UnQual () (Ident () "f")),
                  None,
#endif
                  None,ValueBinder,None,ValueBinder,None,None,None,None,None,
                  LocalValue (SrcLoc "<unknown>.hs" 2 3),
                  LocalValue (SrcLoc "<unknown>.hs" 2 3),
                  None,
                  ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []})
                                                  (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []}) ">"))),
                  ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []})
                                                  (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 9 2 10, srcInfoPoints = []}) ">"))),
                  None,None,None,None,
                  LocalValue (SrcLoc "<unknown>.hs" 2 3),
                  LocalValue (SrcLoc "<unknown>.hs" 2 3),
                  None,
                  ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []})
                                                  (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []}) "+"))),
                  ScopeError (ENotInScope (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []})
                                                  (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 2 17 2 18, srcInfoPoints = []}) "+"))),
                  None,None]
      parsemod s = let ParseOk m = parseModule s in let env = resolve [m] mempty in annotate env m

names3 :: Test
names3 = TestCase $ do
           assertEqual "names3" expected actual
    where
      actual = (mtable "module M   (f)   where\nf x | x > 0 = x + 1\n",
                mtable "module M {-(f)-} where\nf x | x > 0 = x + 1\n")
      mtable s = let ParseOk m = parseModule s in
                 let env = resolve [m] mempty in
                 let itable = importTable env m in
                 moduleTable itable m
      expected =
          let mp = Map.fromList [(Qual () (ModuleName () "M") (Ident () "f"),[Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}]),
                                 (UnQual () (Ident () "f"),[Value {symbolModule = ModuleName () "M", symbolName = Ident () "f"}])] in
          (mp, mp)
