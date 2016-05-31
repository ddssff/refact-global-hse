-- | A type that can be turned into a small subset of the command line
-- options for the ghc command.

{-# LANGUAGE RecordWildCards #-}

module GHC
    ( GHCOpts(..)
    , ghcProcessArgs
    ) where

import Data.Default (Default(def))
import Data.List (intercalate)
import Data.Monoid ((<>))
import Text.PrettyPrint (text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow)

-- | Support a tiny subset of the GHC command line options.
data GHCOpts =
    GHCOpts
    { hsSourceDirs :: [FilePath]
    , cppOptions :: [CPPOption]
    , languageOptions :: [String]
    }

instance Default GHCOpts where
    def = GHCOpts [] [] []

data CPPOption
    = CPPDefine String (Maybe String)
    | CPPUndefine String

instance Pretty CPPOption where
    pPrint (CPPUndefine name) = text $ "-U" <> name
    pPrint (CPPDefine name Nothing) = text $ "-D" <> name
    pPrint (CPPDefine name (Just s)) = text $ "-D" <> name <> "=" <> s

ghcProcessArgs :: GHCOpts -> [String]
ghcProcessArgs (GHCOpts {..}) =
    map prettyShow cppOptions <>
    map ("-X" ++) languageOptions <>
    case hsSourceDirs of
      [] -> []
      xs -> ["-i" <> intercalate ":" xs]
