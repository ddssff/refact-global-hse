{-#LANGUAGE CPP, PackageImports, RecordWildCards #-}
module Names
    ( topDeclExportSpec
    ) where

import Data.List (partition)
import Language.Haskell.Exts.Syntax (Decl)
import Language.Haskell.Exts.Syntax (CName(..), EWildcard(..), ExportSpec(..), QName(..))
import Language.Haskell.Names (Symbol(..))
import ModuleInfo (getTopDeclSymbols', ModuleInfo)

-- | Build an export spec for the symbols created by a Decl.  The
-- getBound function returns the names, and we can get the module
-- name from the decl
topDeclExportSpec :: ModuleInfo () -> Decl () -> Maybe (ExportSpec ())
topDeclExportSpec m d =
    case partition isThing (getTopDeclSymbols' m d) of
      ([], [x]) -> (Just . EVar () . UnQual () . symbolName) x
      ([], []) -> Nothing
      ([x], xs) -> Just (EThingWith () (NoWildcard ()) (UnQual () (symbolName x)) (map symbolCName xs))
      x -> error $ "unexpected decl symbols: " ++ show x
    where
      isThing :: Symbol -> Bool
      isThing (Type {}) = True
      isThing (Data {}) = True
      isThing (NewType {}) = True
      isThing (Class {}) = True
      isThing _ = False

symbolCName :: Symbol -> CName ()
symbolCName x@(Value {}) = varName (UnQual () (symbolName x))
symbolCName x@(Method {}) = varName (UnQual () (symbolName x))
symbolCName x@(Selector {}) = varName (UnQual () (symbolName x))
symbolCName x@(Constructor {}) = conName (UnQual () (symbolName x))
symbolCName x = error $ "toCName: " ++ show x

varName :: QName () -> CName ()
varName (UnQual () name) = VarName () name
varName (Qual () _mname name) = VarName () name
varName x = error $ "varName: " ++ show x

conName :: QName () -> CName ()
conName (UnQual () name) = ConName () name
conName (Qual () _mname name) = ConName () name
conName x = error $ "conName: " ++ show x
