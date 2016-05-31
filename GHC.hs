-- | A type that can be turned into a small subset of the command line
-- options for the ghc command.

{-# LANGUAGE RecordWildCards #-}

module GHC
    ( GHCOpts(..)
    , ghcProcessArgs
    ) where

import CPP
import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Monoid ((<>))

-- | Support a tiny subset of the GHC command line options.
data GHCOpts =
    GHCOpts
    { hsSourceDirs :: [FilePath]
    , cpphsOptions :: CpphsOptions
    , languageOptions :: [String]
    }

instance Default GHCOpts where
    def = GHCOpts [] defaultCpphsOptions []

ghcProcessArgs :: GHCOpts -> [String]
ghcProcessArgs (GHCOpts {..}) =
    map (\(name, s) -> "-D" ++ name ++ if null s then "" else ("=" ++ s)) (defines cpphsOptions) <>
    map ("-X" ++) languageOptions <>
    case hsSourceDirs of
      [] -> []
      xs -> ["-i" <> intercalate ":" xs]
