{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module ModuleInfo
    ( ModuleInfo(..)
    , fullPathOfModuleInfo
    ) where

import qualified Language.Haskell.Exts.Annotated as A (Module(Module), ModuleHead(ModuleHead))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import ModuleKey (moduleFullPath, ModuleKey)
import Utils (EZPrint(ezPrint))

data ModuleInfo =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: A.Module SrcSpanInfo
               , _moduleComments :: [Comment]
               , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               }

instance EZPrint ModuleInfo where
    ezPrint (ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ n _ _)) _ _ _}) = prettyPrint n
    ezPrint (ModuleInfo {_module = A.Module _ Nothing _ _ _}) = "Main"
    ezPrint (ModuleInfo {_module = _}) = error "ezPrint: unexpected module"

fullPathOfModuleInfo :: ModuleInfo -> FilePath
fullPathOfModuleInfo = moduleFullPath . _moduleKey
