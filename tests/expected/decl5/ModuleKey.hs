module ModuleKey(ModuleKey(ModuleKey, _moduleTop, _moduleName)
    , fullPathOfModuleKey
    , moduleKey
    ) where

import Control.Monad (when)
import Data.List (groupBy, intercalate)
import qualified Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (canonicalizePath)
import System.FilePath ((<.>), (</>), joinPath, splitDirectories, splitExtension, splitFileName)


-- A module is uniquely identitifed by its path and name
data ModuleKey =
    ModuleKey { _moduleTop :: FilePath      -- ^ The <dir> for which ghc -i<dir> finds this module
              , _moduleName :: Maybe S.ModuleName
              -- ^ The module name, if it has one.
              } deriving (Eq, Ord, Show)

fullPathOfModuleKey :: ModuleKey -> FilePath
fullPathOfModuleKey (ModuleKey {_moduleTop = top, _moduleName = mname}) =
    top </> maybe "Main" moduleNameToPath mname <.> "hs"
    where
      moduleNameToPath (S.ModuleName name) = (joinPath . filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))) name

-- | Compute the module key from a filepath and the parsed module.
moduleKey :: FilePath -> A.Module SrcSpanInfo -> IO ModuleKey
moduleKey _ (A.XmlPage {}) = error "XmlPage"
moduleKey _ (A.XmlHybrid {}) = error "XmlHybrid"
moduleKey path (A.Module _ Nothing _ _ _) = do
  path' <- canonicalizePath path
  let (top, _sub) = splitFileName path'
  pure $ ModuleKey top Nothing
moduleKey path (A.Module _ (Just (A.ModuleHead _ (A.ModuleName _ name) _ _)) _ _ _) = do
  path' <- canonicalizePath path
  let name' = splitModuleName name
      (path'', _ext) = splitExtension path'
      dirs = splitDirectories path''
      (dirs', name'') = splitAt (length dirs - length name') dirs
  when (name'' /= name') (error $ "Module name mismatch - name: " ++ show name' ++ ", path: " ++ show name'')
  pure $ ModuleKey (joinPath dirs')
                   (Just (S.ModuleName (intercalate "." name'')))
      where
        splitModuleName = filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))
