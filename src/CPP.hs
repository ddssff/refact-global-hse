-- A module copied from hse-cpp, but the return value from
-- parseFileWithCommentsAndCPP includes the preprocessed text.
-- Hopefully I won't need this, but it is useful for debugging.
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module CPP
  ( defaultCpphsOptions
  , GHCOpts(GHCOpts), hc, cppOptions, enabled, hashDefines, hsSourceDirs, ghcOptions
  , applyHashDefine
  , ghcProcessArgs
  , cppIf
  , cppEndif
  , extensionsForHSEParser
  , ghcOptsOptions
  , cabalMacro
  , tests
  ) where

import Control.Lens (makeLenses, view)
import Data.Char (isDigit)
import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Version(Version(Version))
import HashDefine (HashDefine(..), parseHashDefine)
import Language.Haskell.Exts (KnownExtension)
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Preprocessor.Cpphs (CpphsOptions(defines), defaultCpphsOptions)
import Language.Preprocessor.Cpphs ()
import Options.Applicative
import Test.HUnit
import Utils (EZPrint(ezPrint))

-- | Support a tiny subset of the GHC command line options.
data GHCOpts =
    GHCOpts
    { _hc :: String
    , _hsSourceDirs :: [FilePath]
    , _cppOptions :: CpphsOptions
    , _enabled :: [KnownExtension]
    , _hashDefines :: [HashDefine]
    , _ghcOptions :: [String]
    } deriving Show

$(makeLenses ''GHCOpts)

instance Default GHCOpts where
    def = GHCOpts
          { _hc = "ghc"
          , _hsSourceDirs = []
          , _cppOptions = defaultCpphsOptions
          , _enabled = []
          , _hashDefines = []
          , _ghcOptions = [] }

applyHashDefine :: HashDefine -> CpphsOptions -> CpphsOptions
applyHashDefine d x = x {defines = applyHashDefine' d (defines x)}
    where
      applyHashDefine' (AntiDefined {name = n}) xs = filter ((/= n) . fst) xs
      applyHashDefine' (SymbolReplacement {name = n, replacement = r}) xs =
          (n, r) : applyHashDefine' (AntiDefined {name = n, linebreaks = linebreaks d}) xs
      applyHashDefine' _ xs = xs

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
      hds = (++) <$> ((catMaybes . map (parseHashDefine False . prepareDefine)) <$> many (strOption (short 'D' <> help "Add a #define to the compiler options")))
                 <*> ((catMaybes . map (parseHashDefine False . (\s -> ["undef", s]))) <$> many (strOption (short 'U' <> help "Add a #undef to the compiler options")))

      prepareDefine :: String -> [String]
      prepareDefine s = "define" : case break (== '=') s of
                                     (_, "") -> [s]
                                     (name, '=' : val) -> [name, val]
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

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 = TestCase (assertEqual "test1" expected actual)
    where
      expected = Just (SymbolReplacement { name = "MIN_VERSION_base(major1,major2,minor)"
                                         , replacement = " (  (major1) <  4 ||   (major1) = 4 && (major2) <  8 ||   (major1) == 4 && (major2) == 8 && (minor) <= 2)"
                                         , linebreaks = 4})
      -- This doesn't work.  Use cabalMacro for now.
      actual = parseHashDefine True ["define", s]
      s = "#define MIN_VERSION_base(major1,major2,minor) (\\\n" <>
          "  (major1) <  4 || \\\n" <>
          "  (major1) == 4 && (major2) <  8 || \\\n" <>
          "  (major1) == 4 && (major2) == 8 && (minor) <= 2)\n"
      s' = "#define MIN_VERSION_base(major1,major2,minor) (  (major1) <  4 ||   (major1) == 4 && (major2) <  8 ||   (major1) == 4 && (major2) == 8 && (minor) <= 2)"

-- | Return a HashDefine such as those in dist/build/autogen/cabal_macros.h
cabalMacro :: String -> Version -> HashDefine
cabalMacro name (Version branch _tags) =
    SymbolReplacement
      ("MIN_VERSION_" ++ name ++ "(major1,major2,minor)")
      ("((major1)<" ++ show major1 ++ "||(major1)==" ++ show major1 ++ "&&(major2)<" ++ show major2 ++ "||(major1)==" ++ show major1 ++ "&&(major2)==" ++ show major2 ++ "&&(minor)<="++ show minor ++ ")")
      0
    where (major1 : major2 : minor : _) = branch ++ repeat 0
