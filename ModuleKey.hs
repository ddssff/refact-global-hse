{-# LANGUAGE TemplateHaskell #-}

module ModuleKey
    ( ModuleKey(ModuleKey, ModuleFullPath, _moduleTop, _moduleName, _moduleFullPath)
    , moduleFullPath
    , moduleName
    , moduleTop
    ) where

import qualified CPP (BoolOptions(locations), CpphsOptions(boolopts), defaultCpphsOptions, parseFileWithCommentsAndCPP)
import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted as IO (try)
import Control.Lens (makeLenses)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate)
import Data.Set as Set (empty, Set, singleton, union, unions)
import Debug.Trace (trace)
import qualified Language.Haskell.Exts.Annotated as A (Decl(DerivDecl), InstHead(..), InstRule(..), Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName), QName(Qual, UnQual), Type(..))
import Language.Haskell.Exts.Annotated.Simplify as S (sModuleName, sName)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions, parseFilename, fixities), fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(..))
import SrcLoc (fixSpan, textSpan)
import System.Directory (canonicalizePath)
import System.FilePath ((</>), (<.>), joinPath, makeRelative, splitDirectories, splitExtension, splitFileName)
import Text.PrettyPrint.HughesPJClass as PP (Pretty(pPrint), prettyShow, text)
import Utils (EZPrint(ezPrint))

-- A unique identifier of a module.  This can be done in two ways.  If
-- the module has an explict name its full path must be (top </> name
-- <.> ext), where name is constructed from the module name.  If it has no
-- name the module name is "Main" and it can have any path with a valid the path can be any 

-- directory plus the path constructed from   has If a module has a top path, its
-- name must match the remainder of its path.  module is uniquely
-- identitifed by its path and name
data ModuleKey
    = ModuleKey
      { -- | The Hs-Source-Dirs path for which ghc -i<dir>
        -- finds this module (canonicalized)
        _moduleTop :: FilePath,
        -- | The module name, if it has one.
        _moduleName :: S.ModuleName,
        -- | The extension, including the dot
        _moduleExt :: String }
    | ModuleFullPath
      { -- | The full path to the module file, canonicalized.
        _moduleFullPath :: FilePath }
    deriving (Eq, Ord, Show)

moduleFullPath :: ModuleKey -> FilePath
moduleFullPath (ModuleFullPath {_moduleFullPath = x}) = x
moduleFullPath (ModuleKey {_moduleTop = top, _moduleName = S.ModuleName mname}) =
    top </> mname <.> "hs"
    where
      moduleNameToPath (S.ModuleName name) = (joinPath . filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))) name

moduleName :: ModuleKey -> Maybe S.ModuleName
moduleName (ModuleKey {_moduleName = name}) = Just name
moduleName (ModuleFullPath {}) = Nothing

moduleTop :: ModuleKey -> Maybe FilePath
moduleTop (ModuleKey {_moduleTop = x}) = Just x
moduleTop _ = Nothing

instance EZPrint ModuleKey where
    ezPrint (ModuleKey {_moduleName = n}) = prettyPrint n
    ezPrint (ModuleFullPath p) = p

instance Pretty ModuleKey where
    pPrint (ModuleKey {_moduleName = S.ModuleName m}) = text m
    pPrint (ModuleFullPath p) = text ("Main (in " ++ p ++ ")")
