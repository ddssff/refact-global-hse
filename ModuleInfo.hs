{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ScopedTypeVariables, TemplateHaskell #-}

module ModuleInfo
    ( ModuleInfo(..)
    , fullPathOfModuleInfo
    ) where

import Data.Generics
import qualified Language.Haskell.Exts.Annotated as A (Module(Module), ModuleHead(ModuleHead))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import ModuleKey (moduleFullPath, ModuleKey)
import Utils (EZPrint(ezPrint))

data ModuleInfo l =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: A.Module l
               , _moduleComments :: [Comment]
               , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               } deriving (Data, Typeable, Functor)

instance EZPrint (ModuleInfo l) where
    ezPrint (ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ n _ _)) _ _ _}) = prettyPrint n
    ezPrint (ModuleInfo {_module = A.Module _ Nothing _ _ _}) = "Main"
    ezPrint (ModuleInfo {_module = _}) = error "ezPrint: unexpected module"

fullPathOfModuleInfo :: ModuleInfo l -> FilePath
fullPathOfModuleInfo = moduleFullPath . _moduleKey
