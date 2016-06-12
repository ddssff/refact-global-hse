{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Data.Default (def)
import Data.List (intercalate)
import Data.Map.Strict as Map (fromList)
import DeclTests
import ImportTests
import Language.Haskell.Exts -- (parseModule, ParseResult(ParseOk))
import Language.Haskell.Names
import Language.Haskell.Names.ModuleSymbols (moduleTable)
import Language.Haskell.Names.Imports (importTable)
import LoadModule (loadModule)
import ModuleInfo (ModuleInfo(..))
import SrcLoc (debugRender)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (assertEqual, errors, failures, runTestTT, Test(TestCase, TestList))
import Utils

main :: IO ()
main = runTestTT (TestList [importTests, declTests, cpp1]) >>= doCounts
    where
      doCounts counts' = exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)

cpp1 :: Test
cpp1 = TestCase $ do
         (ModuleInfo {_module = m, _moduleText = s, _moduleComments = cs}) <- loadModule def "tests/input/cpp/A.hs"
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
