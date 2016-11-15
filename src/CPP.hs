-- A module copied from hse-cpp, but the return value from
-- parseFileWithCommentsAndCPP includes the preprocessed text.
-- Hopefully I won't need this, but it is useful for debugging.
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module CPP
  ( CPP.defaultParseMode
  , defaultCpphsOptions
  , turnOffLocations
  , GHCOpts(GHCOpts), hc, cppOptions, enabled, hashDefines, hsSourceDirs, ghcOptions
  , applyHashDefine
  , applyHashDefine'
  , ghcProcessArgs
  , cppIf
  , cppEndif
  , extensionsForHSEParser
  , ghcOptsOptions
  , cabalMacro
  , tests
  ) where

import Control.Lens (makeLenses, makeLensesFor, set, view)
import Data.Char (isDigit)
import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Version(Version(Version))
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.Text (Text(parse))
import HashDefine (HashDefine(..), parseHashDefine)
import Language.Haskell.Exts (KnownExtension, ParseMode(..))
import Language.Haskell.Exts.CPP (parseFileContentsWithCommentsAndCPP)
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, fixities, parseFilename))
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Preprocessor.Cpphs (BoolOptions(..), CpphsOptions(..), defaultCpphsOptions, parseOptions, runCpphs)
import Options.Applicative
import Test.HUnit
import Utils (EZPrint(ezPrint))

deriving instance Show ParseMode
deriving instance Eq BoolOptions
deriving instance Eq CpphsOptions

$(makeLensesFor [ {-("infiles", "infilesL")
                , ("outfiles", "outfilesL")
                , ("defines", "definesL")
                , ("includes", "includesL")
                , ("preInclude", "preIncludeL")
                , -} ("boolopts", "booloptsL") ] ''CpphsOptions)
$(makeLensesFor [ ("macros", "macrosL")
                {- , ("locations", "locationsL")
                , ("hashline", "hashlineL")
                , ("pragma", "pragmaL")
                , ("stripEol", "stripEolL")
                , ("stripC89", "stripC89L")
                , ("lang", "langL")
                , ("ansi", "ansiL")
                , ("layout", "layoutL")
                , ("literate", "literateL")
                , ("warnings", "warningsL")-} ] ''BoolOptions)

-- | Support a tiny subset of the GHC command line options.
data GHCOpts =
    GHCOpts
    { _hc :: String
    , _hsSourceDirs :: [FilePath]
    , _cppOptions :: CpphsOptions
    , _enabled :: [KnownExtension] -- FIXME - use the extensions field in ParseMode
    , _hashDefines :: [HashDefine]
    , _ghcOptions :: [String]
    } deriving (Eq, Show)

$(makeLenses ''GHCOpts)

instance Default GHCOpts where
    def = GHCOpts
          { _hc = "ghc"
          , _hsSourceDirs = []
          , _cppOptions = defaultCpphsOptions
          , _enabled = [CPP] -- FIXME - this should come from the source file and/or the cabal file
          , _hashDefines = []
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
applyHashDefine :: HashDefine -> GHCOpts -> GHCOpts
applyHashDefine d x = x { _hashDefines = d : _hashDefines x
                        , _cppOptions = applyHashDefine' d (_cppOptions x) }

applyHashDefine' :: HashDefine -> CpphsOptions -> CpphsOptions
applyHashDefine' d x = x {defines = applyHashDefine'' d (defines x)}
    where
      applyHashDefine'' :: HashDefine -> [(String, String)] -> [(String, String)]
      applyHashDefine'' (AntiDefined {name = n}) xs = filter ((/= n) . fst) xs
      applyHashDefine'' (SymbolReplacement {name = n, replacement = r}) xs =
          (n, r) : applyHashDefine'' (AntiDefined {name = n, linebreaks = linebreaks d}) xs
      applyHashDefine'' _ xs = xs

instance EZPrint GHCOpts where
    ezPrint x = unwords (view hc x : map asArgument (view hashDefines x))

asArgument :: HashDefine -> String
asArgument (AntiDefined{..}) = "-U" <> name
asArgument (SymbolReplacement{..}) =
    "-D" <> name <> if replacement == "" then "" else ("=" <> replacement)
asArgument _ = error $ "Unexpected HashDefine value"

asPredicate :: HashDefine -> String
asPredicate (AntiDefined{..}) = "!defined(" <> name <> ")"
asPredicate (SymbolReplacement{..}) | replacement == "" = "defined(" <> name <> ")"
asPredicate (SymbolReplacement{..}) | all (== '0') replacement = "!" <> name
asPredicate (SymbolReplacement{..}) | all isDigit replacement = name
asPredicate (SymbolReplacement{..}) = name <> " == " <> replacement
asPredicate _ = ""

ghcProcessArgs :: GHCOpts -> [String]
ghcProcessArgs opts =
    map asArgument (view hashDefines opts) <>
    concatMap ppExtension (map EnableExtension (view enabled opts)) <>
    view ghcOptions opts <>
    case view hsSourceDirs opts of
      [] -> []
      xs -> ["-i" <> intercalate ":" xs]

-- | Somewhere there should be a library to do this.
cppIf :: GHCOpts -> [String]
cppIf opts | null (view hashDefines opts) = []
cppIf opts = ["#if " <> intercalate " && " (map asPredicate (view hashDefines opts))]

cppEndif :: GHCOpts -> [String]
cppEndif opts | null (view hashDefines opts) = []
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
    GHCOpts <$> pure "ghc" <*> sds <*> pure defaultCpphsOptions <*> pure [] <*> hds <*> pure []
    where
      sds :: Parser [FilePath]
      sds = many (strOption (short 'i' <> metavar "DIR" <> help "Add a top directory, as in cabal's hs-source-dirs"))
      hds :: Parser [HashDefine]
      hds = (\a b c -> a <> b <> c)
              <$> ((catMaybes . map (parseHashDefine False . prepareDefine)) <$> many (strOption (short 'D' <> help "Add a #define to the compiler options")))
              <*> ((catMaybes . map (parseHashDefine False . prepareUndef)) <$> many (strOption (short 'U' <> help "Add a #undef to the compiler options")))
              <*> ((catMaybes . map cabalMacro') <$> many (strOption (long "package" <> help "define a macro for a package id")))

prepareDefine :: String -> [String]
prepareDefine s = "define" : case break (== '=') s of
                               (_, "") -> [s]
                               (name, '=' : val) -> [name, val]
                               _ -> error "ghcOptsOptions"
prepareUndef :: String -> [String]
prepareUndef s = ["undef", s]

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
cabalMacro :: String -> Version -> HashDefine
cabalMacro name (Version branch _tags) =
    SymbolReplacement
      ("MIN_VERSION_" ++ name ++ "(major1,major2,minor)")
      ("((major1)<" ++ show major1 ++ "||(major1)==" ++ show major1 ++ "&&(major2)<" ++ show major2 ++ "||(major1)==" ++ show major1 ++ "&&(major2)==" ++ show major2 ++ "&&(minor)<="++ show minor ++ ")")
      0
    where (major1 : major2 : minor : _) = branch ++ repeat 0

cabalMacro' :: String -> Maybe HashDefine
cabalMacro' s = fmap (\PackageIdentifier{..} -> cabalMacro (unPackageName pkgName) pkgVersion) (parsePackageIdentifier s)

parsePackageIdentifier :: String -> Maybe PackageIdentifier
parsePackageIdentifier s =
    case filter ((== "") . snd) (readP_to_S parse s) of
      [(x, _)] -> Just x
      _ -> Nothing

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6a, test6b, test6c, test7]

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

test2 :: Test
test2 = TestCase (assertEqual "test2" expected actual)
    where
      expected = PackageIdentifier (PackageName "base") (Version [4,8] [])
      actual = pid
      Just pid = parsePackageIdentifier "base-4.8"

test3 :: Test
test3 = TestCase (assertEqual "test3" expected actual)
    where
      expected = GHCOpts { _hc = "ghc"
                         , _hsSourceDirs = []
                         , _cppOptions =
                             CpphsOptions { infiles = []
                                          , outfiles = []
                                          , defines = [("MIN_VERSION_base(major1,major2,minor)","((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=0)")]
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
                             [SymbolReplacement
                              {name = "MIN_VERSION_base(major1,major2,minor)",
                               replacement = "((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=0)",
                               linebreaks = 0}]
                         , _ghcOptions = [] }
      actual = applyHashDefine hd def
      Just hd = cabalMacro' "base-4.8"

test4 :: Test
test4 = TestCase $ do
          assertEqual "test4"
                          ["define",
                           "MIN_VERSION_base(major1,major2,minor)",
                           "((major1)<4||(major1)==4&&(major2)<8||(major1)==4&&(major2)==8&&(minor)<=2)"]
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
