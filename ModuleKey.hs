{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module ModuleKey
    ( ModuleKey(ModuleKey, _moduleTop, _moduleName, _moduleExt, ModuleFullPath, _moduleFullPath)
    , moduleFullPath
    , moduleName
    , moduleTop
    ) where

import Data.Generics (Data, Typeable)
import Data.List (groupBy, intercalate)
import Language.Haskell.Exts.Annotated as A (ModuleName(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.FilePath ((<.>), (</>))
import Text.PrettyPrint.HughesPJClass as PP (Pretty(pPrint), text)
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
        _moduleName :: A.ModuleName (),
        -- | The extension, including the dot
        _moduleExt :: String }
    | ModuleFullPath
      { -- | The full path to the module file, canonicalized.
        _moduleFullPath :: FilePath }
    deriving (Eq, Ord, Show, Data, Typeable)

moduleFullPath :: ModuleKey -> FilePath
moduleFullPath (ModuleFullPath {_moduleFullPath = x}) = x
moduleFullPath (ModuleKey {_moduleTop = top, _moduleName = A.ModuleName () mname}) =
    top </>
    (intercalate "/" . filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))) mname <.>
    "hs"

moduleName :: ModuleKey -> Maybe (A.ModuleName ())
moduleName (ModuleKey {_moduleName = name}) = Just name
moduleName (ModuleFullPath {}) = Nothing

moduleTop :: ModuleKey -> Maybe FilePath
moduleTop (ModuleKey {_moduleTop = x}) = Just x
moduleTop _ = Nothing

instance EZPrint ModuleKey where
    ezPrint (ModuleKey {_moduleName = n, _moduleTop = top}) = prettyPrint n ++ " (top=" ++ show top ++ ")"
    ezPrint (ModuleFullPath p) = p

instance Pretty ModuleKey where
    pPrint (ModuleKey {_moduleName = A.ModuleName () m}) = text m
    pPrint (ModuleFullPath p) = text ("Main (in " ++ p ++ ")")
