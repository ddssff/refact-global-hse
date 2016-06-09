-- A module copied from hse-cpp, but the return value from
-- parseFileWithCommentsAndCPP includes the preprocessed text.
-- Hopefully I won't need this, but it is useful for debugging.
{-# LANGUAGE RecordWildCards #-}
module CPP
  ( parseFileWithCommentsAndCPP
  , defaultCpphsOptions
  , GHCOpts(GHCOpts, hc, hsSourceDirs, cppOptions, enabled)
  , ghcProcessArgs
  , extensionsForHSEParser
  ) where

import Data.Default (Default(def))
import Data.List (intercalate, isSuffixOf)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Annotated (Comment, impliesExts, KnownExtension(CPP), Module, ParseMode(baseLanguage, extensions, ignoreLanguagePragmas, parseFilename), parseModuleWithComments, ParseResult, readExtensions, SrcSpanInfo, toExtensionList)
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Preprocessor.Cpphs (BoolOptions(hashline, locations, stripC89, stripEol), CpphsOptions(boolopts, defines), runCpphs)
import qualified Language.Preprocessor.Cpphs as Orig (defaultCpphsOptions)
import Language.Preprocessor.Unlit (unlit)

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
  in p { extensions = exts ++ extraExts
       , ignoreLanguagePragmas = False
       , baseLanguage = bLang
       }

cpp :: CpphsOptions -> ParseMode -> String -> IO String
cpp cppopts p str
  | CPP `elem` impliesExts (toExtensionList (baseLanguage p) (extensions p))
  = runCpphs cppopts (parseFilename p) str
  | otherwise = return str

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
    { hc :: String
    , hsSourceDirs :: [FilePath]
    , cppOptions :: CpphsOptions
    , enabled :: [KnownExtension]
    }

instance Default GHCOpts where
    def = GHCOpts
          { hc = "ghc"
          , hsSourceDirs = []
          , cppOptions = defaultCpphsOptions
          , enabled = [] }

ghcProcessArgs :: GHCOpts -> [String]
ghcProcessArgs (GHCOpts {..}) =
    map (\(name, s) -> "-D" ++ name ++ if null s then "" else ("=" ++ s)) (defines cppOptions) <>
    concatMap ppExtension (map EnableExtension enabled) <>
    case hsSourceDirs of
      [] -> []
      xs -> ["-i" <> intercalate ":" xs]

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
ppExtension (EnableExtension x) = ["-X"++ show x]
ppExtension _ = []
