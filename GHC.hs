-- | A type that can be turned into a small subset of the command line
-- options for the ghc command.

{-# LANGUAGE RecordWildCards #-}

module GHC
    ( GHCOpts(..)
    , ghcProcessArgs
    , extensionsForHSEParser
    ) where

import CPP
import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))

-- | Support a tiny subset of the GHC command line options.
data GHCOpts =
    GHCOpts
    { hc :: String
    , hsSourceDirs :: [FilePath]
    , cppOptions :: CpphsOptions
    , extensions :: [KnownExtension]
    }

instance Default GHCOpts where
    def = GHCOpts
          { hc = "ghc"
          , hsSourceDirs = []
          , cppOptions = defaultCpphsOptions
          , extensions = [] }

ghcProcessArgs :: GHCOpts -> [String]
ghcProcessArgs (GHCOpts {..}) =
    map (\(name, s) -> "-D" ++ name ++ if null s then "" else ("=" ++ s)) (defines cppOptions) <>
    concatMap ppExtension (map EnableExtension extensions) <>
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
