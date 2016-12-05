-- A module copied from hse-cpp, but the return value from
-- parseFileWithCommentsAndCPP includes the preprocessed text.
-- Hopefully I won't need this, but it is useful for debugging.
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module CPP
  ( CPP.defaultParseMode
  , defaultCpphsOptions
  , turnOffLocations
  , GHCOpts(GHCOpts), hc, cppOptions, enabled, hashUndefs, hsSourceDirs, ghcOptions
  , hashDefines
  , HashDef(HashDef, HashUndef)
  , infilesL, outfilesL, definesL, includesL, preIncludeL, booloptsL
  , applyHashDefine
  , applyHashDefine'
  , ghcProcessArgs
  , cppIf
  , cppEndif
  , extensionsForHSEParser
  , ghcOptsOptions
  , cabalMacro
  , cabalMacro'
  , tests
  ) where

import Control.Lens (_2, makeLenses, makeLensesFor, over, set, view)
import Data.Char (isDigit)
import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Version (Version(Version, versionBranch))
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Text (Text(parse))
import Language.Haskell.Exts (KnownExtension, ParseMode(..))
import Language.Haskell.Exts.CPP (parseFileContentsWithCommentsAndCPP)
import Language.Haskell.Exts.Comments (Comment(Comment))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(..))
import Language.Haskell.Exts.SrcLoc (SrcSpan(SrcSpan), SrcSpanInfo(SrcSpanInfo, srcInfoPoints, srcInfoSpan))
import Language.Haskell.Exts.Syntax (Decl(PatBind, TypeSig), Exp(InfixApp, Lit, Var), ExportSpec(EVar), ExportSpecList(ExportSpecList), Literal(Int), Module(Module), ModuleHead(ModuleHead), ModuleName(ModuleName), Name(Ident, Symbol), Pat(PVar), QName(UnQual), QOp(QVarOp), Rhs(UnGuardedRhs), Type(TyCon))
import Language.Preprocessor.Cpphs (BoolOptions(..), CpphsOptions(..), defaultCpphsOptions, parseOptions, runCpphs, runCpphsReturningSymTab)
import Options.Applicative ((<>), Alternative(many), help, long, metavar, Parser, short, strOption)
import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Utils (EZPrint(ezPrint))

deriving instance Show ParseMode
deriving instance Eq BoolOptions
deriving instance Eq CpphsOptions

$(makeLensesFor [ ("infiles", "infilesL")
                , ("outfiles", "outfilesL")
                , ("defines", "definesL")
                , ("includes", "includesL")
                , ("preInclude", "preIncludeL")
                , ("boolopts", "booloptsL") ] ''CpphsOptions)
$(makeLensesFor [ ("macros", "macrosL")
                , ("locations", "locationsL")
                , ("hashline", "hashlineL")
                , ("pragma", "pragmaL")
                , ("stripEol", "stripEolL")
                , ("stripC89", "stripC89L")
                , ("lang", "langL")
                , ("ansi", "ansiL")
                , ("layout", "layoutL")
                , ("literate", "literateL")
                , ("warnings", "warningsL") ] ''BoolOptions)

-- | Simple representation of #define and #undef
data HashDef = HashDef String String | HashUndef String deriving (Show, Eq, Ord)

-- | Support a tiny subset of the GHC command line options.
data GHCOpts =
    GHCOpts
    { _hc :: String
    , _hsSourceDirs :: [FilePath]
    , _cppOptions :: CpphsOptions
    , _enabled :: [KnownExtension] -- FIXME - use the extensions field in ParseMode
    , _hashUndefs :: [String] -- The defines are in cppOptions
    , _ghcOptions :: [String]
    } deriving (Eq, Show)

$(makeLenses ''GHCOpts)

instance Default GHCOpts where
    def = GHCOpts
          { _hc = "ghc"
          , _hsSourceDirs = []
          , _cppOptions = defaultCpphsOptions
          , _enabled = [CPP] -- FIXME - this should come from the source file and/or the cabal file
          , _hashUndefs = []
          , _ghcOptions = [] }

defaultParseMode :: GHCOpts -> String -> ParseMode
defaultParseMode opts path =
    Exts.defaultParseMode {Exts.extensions = map EnableExtension (view enabled opts ++ extensionsForHSEParser),
                           Exts.parseFilename = path,
                           Exts.fixities = Nothing }

-- | Turn of the locations flag.  This means simple #if macros will not
-- affect the line numbers of the output text, so we can use the
-- resulting SrcSpan info on the original text.  Macro expansions
-- could still mess this up.
turnOffLocations :: CpphsOptions -> CpphsOptions
turnOffLocations opts = opts { boolopts = (boolopts opts) {locations = False } }

{-
cpphsOptions :: CpphsOptions
cpphsOptions =
    CPP.defaultCpphsOptions
    { boolopts =
          ( boolopts CPP.defaultCpphsOptions )
          { locations = False
      }
    }
-}

-- applyHashDefine :: HashDefine -> CpphsOptions -> CpphsOptions
-- applyHashDefine d x = x {defines = applyHashDefine' d (defines x)}

applyHashDefine :: HashDef -> GHCOpts -> GHCOpts
applyHashDefine d x =
    let x' = over hashUndefs (case d of
                                HashUndef n -> (n :)
                                _ -> id) x in
    over (cppOptions . definesL) (applyHashDefine'' d) x'

applyHashDefine' :: HashDef -> CpphsOptions -> CpphsOptions
applyHashDefine' d x = x {defines = applyHashDefine'' d (defines x)}

applyHashDefine'' :: HashDef -> [(String, String)] -> [(String, String)]
applyHashDefine'' (HashUndef n) xs = filter ((/= n) . fst) xs
applyHashDefine'' (HashDef n r) xs = (n, r) : applyHashDefine'' (HashUndef n) xs
applyHashDefine'' _ xs = xs

instance EZPrint GHCOpts where
    ezPrint x = unwords (view hc x : map asArgument (hashDefines x))

hashDefines :: GHCOpts -> [HashDef]
hashDefines opts = map (uncurry HashDef) (view (cppOptions . definesL) opts) <> map HashUndef (view hashUndefs opts)

asArgument :: HashDef -> String
asArgument (HashUndef name) = "-U" <> name
asArgument (HashDef name replacement) =
    "-D" <> name <> if replacement == "" then "" else ("=" <> replacement)
asArgument _ = error $ "Unexpected HashDefine value"

-- | Build the argument list for GHC from the options in GHCOpts.
ghcProcessArgs :: GHCOpts -> [String]
ghcProcessArgs opts =
    map asArgument (hashDefines opts) <>
    concatMap ppExtension (map EnableExtension (view enabled opts)) <>
    view ghcOptions opts <>
    case view hsSourceDirs opts of
      [] -> []
      xs -> ["-i" <> intercalate ":" xs]

-- | Build the predicate of an #if line from the options in GHCOpts.
-- This is used to protect inserted code, since we only know what is
-- correct for the options under which ghc is running.
cppIf :: GHCOpts -> [String]
cppIf opts | null (hashDefines opts) = []
cppIf opts = ["#if " <> intercalate " && " (map asPredicate (hashDefines opts))]
    where
      asPredicate :: HashDef -> String
      asPredicate (HashDef name replacement) | replacement == "" = "defined(" <> name <> ")"
      asPredicate (HashDef name replacement) | all (== '0') replacement = "!" <> name
      asPredicate (HashDef name replacement) | all isDigit replacement = name
      asPredicate (HashDef name replacement) = name <> " == " <> replacement
      asPredicate (HashUndef name) = "!defined(" <> name <> ")"

cppEndif :: GHCOpts -> [String]
cppEndif opts | null (hashDefines opts) = []
cppEndif _ = ["#endif"]

-- | From hsx2hs, but removing Arrows because it makes test case
-- fold3c and others fail.  Maybe we should parse the headers and then
-- use the options there?  There are functions to do this.
extensionsForHSEParser :: [KnownExtension]
extensionsForHSEParser =
    [ RecursiveDo, ParallelListComp, MultiParamTypeClasses, FunctionalDependencies, RankNTypes, ExistentialQuantification
    , ScopedTypeVariables, ImplicitParams, FlexibleContexts, FlexibleInstances, EmptyDataDecls, KindSignatures
    , BangPatterns, TemplateHaskell, ForeignFunctionInterface, {- Arrows, -} DeriveGeneric, NamedFieldPuns, PatternGuards
    , MagicHash, TypeFamilies, StandaloneDeriving, TypeOperators, RecordWildCards, GADTs, UnboxedTuples
    , PackageImports, QuasiQuotes, {-TransformListComp,-} ViewPatterns, {-XmlSyntax, RegularPatterns,-} TupleSections
    , ExplicitNamespaces
    ]

ppExtension :: Extension -> [String]
ppExtension (EnableExtension x) = ["-X" <> show x]
ppExtension _ = []


ghcOptsOptions :: Parser GHCOpts
ghcOptsOptions =
    GHCOpts <$> pure "ghc" <*> sds <*> cpp <*> pure [] <*> hus <*> pure []
    where
      sds :: Parser [FilePath]
      sds = many (strOption (short 'i' <> metavar "DIR" <> help "Add a top directory, as in cabal's hs-source-dirs"))
      cpp :: Parser CpphsOptions
      cpp = (\a b -> defaultCpphsOptions {defines = a <> b}) <$> hds <*> cms
      hds :: Parser [(String, String)]
      hds = map prepareDefine <$> many (strOption (short 'D' <> help "Add a #define to the compiler options"))
      cms :: Parser [(String, String)]
      cms = (catMaybes . map cabalMacro') <$> many (strOption (long "package" <> help "define a macro for a package id"))
{-
      hds :: Parser [(String, String)]
      hds = (\a b c -> a <> b <> c)
              <$> 
              <*> (
              <*> (map HashUndef <$> )
-}
      hus :: Parser [String]
      hus = many (strOption (short 'U' <> help "Add a #undef to the compiler options"))

prepareDefine :: String -> (String, String)
prepareDefine s = case break (== '=') s of
                    (_, "") -> (s, "")
                    (name, '=' : val) -> (name, val)
                    _ -> error "ghcOptsOptions"

{-
ghcOptsOptions' :: [OptDescr (GHCOpts -> GHCOpts)]
ghcOptsOptions' =
    [ Option "i" ["hs-source-dir"] (ReqArg (\s -> over hsSourceDirs ((groupOn (== ':') s) <>)) "DIR")
             "Add a directory to the haskell source path"
    , Option "D" [] (ReqArg (\s -> let Just x = parseHashDefine False
                                                                ("define" :
                                                                 case break (== '=') s of
                                                                   (_, "") -> [s]
                                                                   (name, '=' : value) -> [name, value]
                                                                   _ -> error "ghcOptsOptions") in
                                   over hashDefines (x :)) "NAME")
             "Add a #define to the compiler options"
    , Option "U" [] (ReqArg (\s -> let Just x = parseHashDefine False ["undef", s] in
                                   over hashDefines (x :)) "NAME")
             "Add a #define to the compiler options" ]
-}

-- | Return a HashDefine such as those in dist/build/autogen/cabal_macros.h
cabalMacro :: PackageIdentifier -> (String, String)
cabalMacro (PackageIdentifier{pkgName = PackageName {unPackageName = name}, pkgVersion = Version {versionBranch = branch}}) =
    ("MIN_VERSION_" ++ fmap dashToUnderscore name ++ "(major1,major2,minor)",
     "((major1)<"  ++ show major1 ++
     "||(major1)==" ++ show major1 ++ "&&(major2)<"  ++ show major2 ++
     "||(major1)==" ++ show major1 ++ "&&(major2)==" ++ show major2 ++ "&&(minor)<=" ++ show minor ++ ")")
    where (major1 : major2 : minor : _) = branch ++ repeat 0
          dashToUnderscore '-' = '_'
          dashToUnderscore c = c

cabalMacro' :: String -> Maybe (String, String)
cabalMacro' s = fmap cabalMacro (parsePackageIdentifier s)

parsePackageIdentifier :: String -> Maybe PackageIdentifier
parsePackageIdentifier s =
    case filter ((== "") . snd) (readP_to_S parse s) of
      [(x, _)] -> Just x
      _ -> Nothing

tests :: Test
tests = TestList [{-test1,-} test2, {-test3,-} test4, test5, test6a, test6b, test6c, test7, {-test9,-} test10]

#if 0
test1 :: Test
test1 = TestCase (assertEqual "test1" expected actual)
    where
      expected = [ Just (AntiDefined {name = "FOO", linebreaks = 0})
                 , Just (SymbolReplacement {name = "BAR", replacement = "", linebreaks = 0})
                 , Just (SymbolReplacement {name = "BAZ", replacement = "1", linebreaks = 0})
                 , Just (SymbolReplacement {name = "MIN_VERSION_base(major1,major2,minor)",
                                            replacement = "(\\\n  (major1) <  4 || \\\n  (major1) == 4 && (major2) <  8 || \\\n  (major1) == 4 && (major2) == 8 && (minor) <= 2)\n",
                                            linebreaks = 4}) ]
      -- This doesn't work.  Use cabalMacro for now.
      actual = [ parseHashDefine True ["undef", "FOO"]
               , parseHashDefine True ["define", "BAR"]
               , parseHashDefine True ["define", "BAZ", "1"]
               , parseHashDefine True ["define",
                                       "MIN_VERSION_base(major1,major2,minor)",
                                       ("(\\\n" <>
                                        "  (major1) <  4 || \\\n" <>
                                        "  (major1) == 4 && (major2) <  8 || \\\n" <>
                                        "  (major1) == 4 && (major2) == 8 && (minor) <= 2)\n") ] ]
#endif

test2 :: Test
test2 = TestCase (assertEqual "test2" expected actual)
    where
      expected = PackageIdentifier (PackageName "base") (Version [4,8] [])
      actual = pid
      Just pid = parsePackageIdentifier "base-4.8"

#if 0
test3 :: Test
test3 = TestCase (assertEqual "test3" expected actual)
    where
      expected = GHCOpts { _hc = "ghc"
                         , _hsSourceDirs = []
                         , _cppOptions =
                             CpphsOptions { infiles = []
                                          , outfiles = []
                                          , defines = []
                                          , includes = []
                                          , preInclude = []
                                          , boolopts =
                                              BoolOptions { macros = True
                                                          , locations = True
                                                          , hashline = True -- False
                                                          , pragma = False
                                                          , stripEol = False
                                                          , stripC89 = False -- True
                                                          , lang = True
                                                          , ansi = False
                                                          , layout = False
                                                          , literate = False
                                                          , warnings = True } }
                         , _enabled = [CPP]
                         , _hashDefines =
                             [MacroExpansion {name = "MIN_VERSION_base",
                                              arguments = ["major1","major2","minor"],
                                              expansion = [(Text,"(("),(Arg,"major1"),(Text,")<4||("),(Arg,"major1"),(Text,")==4&&("),
                                                           (Arg,"major2"),(Text,")<8||("),(Arg,"major1"),(Text,")==4&&("),
                                                           (Arg,"major2"),(Text,")==8&&("),(Arg,"minor"),(Text,")<=0)")],
                                              linebreaks = 0}]
                         , _ghcOptions = [] }
      actual = applyHashDefine hd def
      Just hd = cabalMacro' "base-4.8"
#endif

test4 :: Test
test4 = TestCase $ do
          assertEqual "test4"
                          ("MIN_VERSION_base(major1,major2,minor)",
                           "((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)")
                          (prepareDefine "MIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)")

test5 :: Test
test5 = TestCase $ do
          assertEqual "test5" expected actual
    where
      expected = Right (CpphsOptions {infiles = [], outfiles = [], defines = [("FOO","1")], includes = [], preInclude = [],
                                      boolopts = BoolOptions {macros = True, locations = True, hashline = True,
                                                              pragma = False, stripEol = False, stripC89 = False,
                                                              lang = True, ansi = False, layout = False, literate = False,
                                                              warnings = True}})
      actual = parseOptions ["-DFOO"]

test6a :: Test
test6a = TestCase $ do
          let Right opts = parseOptions ["-DFOO", "-DMIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)"]
              expected = output1
          actual <- runCpphs opts "<input>" (input1 "4,8,0")
          assertEqual "test6a" expected actual

test6b :: Test
test6b = TestCase $ do
          let Right opts = parseOptions ["-DFOO", "-DMIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)"]
              expected = output2
          actual <- runCpphs opts "<input>" (input1 "4,8,3")
          assertEqual "test6b" expected actual

test6c :: Test
test6c = TestCase $ do
          let opts :: CpphsOptions
              Right opts = parseOptions ["-DFOO", "-DMIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)", "--nomacro"]
              opts' = set (booloptsL . macrosL) False opts
              expected = output3
          actual <- runCpphs opts' "<input>" (input1 "4,8,3")
          assertEqual "test6c" expected actual

test7 :: Test
test7 = TestCase $ do
          let ghcopts = def
              pmode = CPP.defaultParseMode ghcopts "<input>"
              cppopts :: CpphsOptions
              Right cppopts = parseOptions ["-DFOO", "-DMIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)", "--nomacro", "--noline"]
              cppopts' = set (booloptsL . macrosL) False cppopts
              expected = [(Module
                           (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 2 1 17 1, srcInfoPoints = [SrcSpan "<input>" 2 1 2 1,SrcSpan "<input>" 2 1 2 1,SrcSpan "<input>" 9 1 9 1,SrcSpan "<input>" 10 1 10 1,SrcSpan "<input>" 13 1 13 1,SrcSpan "<input>" 14 1 14 1,SrcSpan "<input>" 17 1 17 1,SrcSpan "<input>" 17 1 17 1]})
                           (Just
                            (ModuleHead
                             (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 2 1 7 12, srcInfoPoints = [SrcSpan "<input>" 2 1 2 7,SrcSpan "<input>" 7 7 7 12]})
                             (ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 2 8 2 10, srcInfoPoints = []}) "M1")
                             Nothing
                             (Just (ExportSpecList (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 3 5 7 6, srcInfoPoints = [SrcSpan "<input>" 3 5 3 6,SrcSpan "<input>" 5 5 5 6,SrcSpan "<input>" 7 5 7 6]}) [EVar (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 3 7 3 9, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 3 7 3 9, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 3 7 3 9, srcInfoPoints = []}) "s1")),EVar (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 5 7 5 9, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 5 7 5 9, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 5 7 5 9, srcInfoPoints = []}) "s2"))]))))
                           []
                           []
                           [TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 9 1 9 10, srcInfoPoints = [SrcSpan "<input>" 9 4 9 6]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 9 1 9 3, srcInfoPoints = []}) "s1"] (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 9 7 9 10, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 9 7 9 10, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 9 7 9 10, srcInfoPoints = []}) "Int"))),
                            PatBind (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 10 1 10 7, srcInfoPoints = []}) (PVar (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 10 1 10 3, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 10 1 10 3, srcInfoPoints = []}) "s1")) (UnGuardedRhs (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 10 4 10 7, srcInfoPoints = [SrcSpan "<input>" 10 4 10 5]}) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 10 6 10 7, srcInfoPoints = []}) (Int (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 10 6 10 7, srcInfoPoints = []}) 1 "1"))) Nothing,
                            TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 13 1 13 10, srcInfoPoints = [SrcSpan "<input>" 13 4 13 6]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 13 1 13 3, srcInfoPoints = []}) "s2"] (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 13 7 13 10, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 13 7 13 10, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 13 7 13 10, srcInfoPoints = []}) "Int"))),
                            PatBind (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 1 14 12, srcInfoPoints = []}) (PVar (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 1 14 3, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 1 14 3, srcInfoPoints = []}) "s2")) (UnGuardedRhs (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 4 14 12, srcInfoPoints = [SrcSpan "<input>" 14 4 14 5]}) (InfixApp (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 6 14 12, srcInfoPoints = []}) (Var (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 6 14 8, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 6 14 8, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 6 14 8, srcInfoPoints = []}) "s1"))) (QVarOp (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 9 14 10, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 9 14 10, srcInfoPoints = []}) (Symbol (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 9 14 10, srcInfoPoints = []}) "+"))) (Lit (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 11 14 12, srcInfoPoints = []}) (Int (SrcSpanInfo {srcInfoSpan = SrcSpan "<input>" 14 11 14 12, srcInfoPoints = []}) 1 "1")))) Nothing],
                           [Comment False (SrcSpan "<input>" 1 1 1 45) " Test moving s2 to a place that imports it"])]
          (parsed, comments) <- Exts.fromParseResult <$> parseFileContentsWithCommentsAndCPP cppopts' pmode (input1 "4,8,0")
          assertEqual "test7" expected [(parsed, comments)]

input1 :: String -> String
input1 basever =
    unlines
        [ "-- Test moving s2 to a place that imports it"
        , "module M1"
        , "    ( s1"
        , "#if MIN_VERSION_base(" ++ basever ++ ")"
        , "    , s2"
        , "#endif"
        , "    ) where"
        , ""
        , "s1 :: Int"
        , "s1 = 1"
        , ""
        , "#if FOO"
        , "s2 :: Int"
        , "s2 = s1 + 1"
        , "#endif" ]

output1 =
    unlines
        [ "#line 1 \"<input>\""
        , "-- Test moving s2 to a place that imports it"
        , "module M1"
        , "    ( s1"
        , ""
        , "    , s2"
        , ""
        , "    ) where"
        , ""
        , "s1 :: Int"
        , "s1 = 1"
        , ""
        , ""
        , "s2 :: Int"
        , "s2 = s1 + 1"
        , "" ]

output2 =
    unlines
        [ "#line 1 \"<input>\""
        , "-- Test moving s2 to a place that imports it"
        , "module M1"
        , "    ( s1"
        , ""
        , ""
        , ""
        , "    ) where"
        , ""
        , "s1 :: Int"
        , "s1 = 1"
        , ""
        , ""
        , "s2 :: Int"
        , "s2 = s1 + 1"
        , "" ]

output3 = output2 ++ "\n"

{-
test7 :: Test
test7 = TestCase $ do
          let expected = input1 "4,8,2"
              path = "tests/input/simple5/M1.hs"
              hd1 :: HashDefine
              Just hd1 = (parseHashDefine False . prepareDefine) "FOO"
              hd2 :: HashDefine
              Just hd2 = (parseHashDefine False . prepareDefine) "MIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)"
              let Right cppopts = parseOptions ["-DFOO", "-DMIN_VERSION_base(major1,major2,minor)=((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)"]
              opts :: GHCOpts
              opts = set cppOptions cppopts defFalse $ foldr applyHashDefine def [hd1, hd2]
              parseMode = CPP.defaultParseMode opts path
          rawStr <- readFile path
          -- let cppMode = updateExtensions parseMode rawStr
          -- trace ("cppOptions: " ++ show (_cppOptions opts)) (pure ())
          -- trace ("cppMode: " ++ show cppMode) (pure ())
          trace ("opts: " ++ show opts) (pure ())
          p <- parseFileContentsWithCommentsAndCPP (_cppOptions opts) parseMode rawStr
          case p of
            ParseOk r -> assertEqual "test7" Nothing (Just r)
            ParseFailed l e -> error (show l ++ ": " ++ show e)
          -- processedSrc <- cpp (_cppOptions opts) cppMode rawStr
-}

#if 0
testHashDefine :: HashDefine
testHashDefine =
    MacroExpansion {name = "MIN_VERSION_base",
                    arguments = ["major1","major2","minor"],
                    expansion = [(Text,"(("),(Arg,"major1"),(Text,")<4||("),(Arg,"major1"),(Text,")==4&&("),
                                 (Arg,"major2"),(Text,")<9||("),(Arg,"major1"),(Text,")==4&&("),
                                 (Arg,"major2"),(Text,")==9&&("),(Arg,"minor"),(Text,")<=0)")],
                    linebreaks = 0}

test9 :: Test
test9 = TestCase $
        assertEqual "test9"
          "((4)<4||(4)==4&&(8)<9||(4)==4&&(8)==9&&(0)<=0)"
          (expandMacro installed minVersion True)
    where
      installed = cabalMacro "base" (Version [4,9,0] [])
      minVersion = ["4","8","0"]
#endif

-- | Parse cabal_macros.h and apply to a piece of sample text
test10 :: Test
test10 = TestCase $ do
           -- The macro expander doesn't work if there are newlines in the
           -- expansion, so we need to set the layout flag to True.  But it
           -- doesn't seem to work, so I filter it below.
           let opts1 = {-set (booloptsL . layoutL) True-} defaultCpphsOptions
           (_, defs) <- readFile "dist/build/autogen/cabal_macros.h" >>=
                        runCpphsReturningSymTab opts1 "cabal_macros.h"
           let opts2 = opts1 {defines = map (over _2 (filter (/= '\n'))) defs}
           result1 <- runCpphs opts2 "Find.hs"
                        (unlines ["#if !MIN_VERSION_base(4,12,0)",
                                  "import Control.Applicative (Applicative)",
                                  "#endif"])
           result2 <- runCpphs opts2 "Find.hs"
                        (unlines ["#if !MIN_VERSION_base(4,5,0)",
                                  "import Control.Applicative (Applicative)",
                                  "#endif"])
           assertEqual "test10"
             ("#line 1 \"Find.hs\"\n\nimport Control.Applicative (Applicative)\n\n",
              "#line 1 \"Find.hs\"\n\n\n\n")
             (result1, result2)


