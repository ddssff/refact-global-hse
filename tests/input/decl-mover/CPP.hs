-- A module copied from hse-cpp, but the return value from
-- parseFileWithCommentsAndCPP includes the preprocessed text.
-- Hopefully I won't need this, but it is useful for debugging.
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module CPP
  ( parseFileWithCommentsAndCPP
  , defaultCpphsOptions
  , GHCOpts(GHCOpts), hc, cppOptions, enabled, hashDefines, hsSourceDirs, ghcOptions
  , applyHashDefine
  , ghcProcessArgs
  , cppIf
  , cppEndif
  , extensionsForHSEParser
  , ghcOptsOptions
  ) where

import Control.Lens (makeLenses, view)
import Data.Char (isDigit)
import Data.Default (Default(def))
import Data.List (intercalate, isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import HashDefine (HashDefine(..), parseHashDefine)
import Language.Haskell.Exts (Comment, impliesExts, KnownExtension(CPP), Module, ParseMode(baseLanguage, extensions, ignoreLanguagePragmas, parseFilename), parseModuleWithComments, ParseResult, readExtensions, SrcSpanInfo, toExtensionList)
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Preprocessor.Cpphs (BoolOptions(hashline, locations, stripC89, stripEol), CpphsOptions(boolopts, defines), runCpphs)
import qualified Language.Preprocessor.Cpphs as Orig (defaultCpphsOptions)
import Language.Preprocessor.Unlit (unlit)
import Options.Applicative
--import System.Console.GetOpt
import Utils (EZPrint(ezPrint))

parseFileWithCommentsAndCPP ::  CpphsOptions -> ParseMode -> FilePath
                      -> IO (ParseResult (Module SrcSpanInfo, [Comment], String))
parseFileWithCommentsAndCPP cppopts parseMode0 file = do
    content <- readFile file
    parseFileContentsWithCommentsAndCPP cppopts parseMode content
  where
    parseMode = parseMode0 { parseFilename = file }

parseFileContentsWithCommentsAndCPP
    :: CpphsOptions -> ParseMode -> String
    -> IO (ParseResult (Module SrcSpanInfo, [Comment], String))
parseFileContentsWithCommentsAndCPP cppopts p rawStr = do
    let file = parseFilename p
        md = delit file rawStr
        cppMode = updateExtensions p md
    processedSrc <- cpp cppopts cppMode md
    let finalMode = updateExtensions cppMode processedSrc
    return $ parseModuleWithComments finalMode processedSrc >>= \(m, cs) -> pure (m, cs, processedSrc)

updateExtensions :: ParseMode -> String -> ParseMode
updateExtensions p modname =
  let oldLang = baseLanguage p
      exts = extensions p
      (bLang, extraExts) =
          case (ignoreLanguagePragmas p, readExtensions modname) of
            (False, Just (mLang, es)) ->
                 (case mLang of {Nothing -> oldLang;Just newLang -> newLang}, es)
            _ -> (oldLang, [])
  in p { extensions = exts <> extraExts
       , ignoreLanguagePragmas = False
       , baseLanguage = bLang
       }

cpp :: CpphsOptions -> ParseMode -> String -> IO String
cpp cppopts p s
  | CPP `elem` impliesExts (toExtensionList (baseLanguage p) (extensions p))
  = runCpphs cppopts (parseFilename p) s
  | otherwise = return s

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id

defaultCpphsOptions :: CpphsOptions
defaultCpphsOptions =
  Orig.defaultCpphsOptions
  { boolopts = (boolopts Orig.defaultCpphsOptions)
      { locations = True
      , stripC89 = True
      , stripEol = False
      , hashline = False
      }
  }


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
