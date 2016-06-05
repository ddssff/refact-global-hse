{-#LANGUAGE CPP, RecordWildCards #-}
module Names
    ( exportSpec
    ) where

import Data.Data (Data)
import Data.List (nub, partition)
import Data.Map as Map (lookup)
import Data.Maybe (mapMaybe)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Simplify (sName)
import Language.Haskell.Exts.Syntax (ExportSpec(..), QName(..), CName(..))
import Language.Haskell.Names (Symbol(..))
import Language.Haskell.Names.GetBound (GetBound(getBound))
import Language.Haskell.Names.GlobalSymbolTable as Global
import LoadModule (Annot)

-- | Build an export spec for the symbols created by a Decl.  The
-- getBound function returns the names, and we can get the module
-- name from the decl
exportSpec :: (Eq l, Data l) => Global.Table -> A.Decl l -> Maybe ExportSpec
exportSpec gtable d =
    case partition isThing (concat (mapMaybe (`Map.lookup` gtable) (map (UnQual . sName) (nub (getBound gtable d)) :: [QName]))) of
      ([], [x]) -> Just (EVar (toQName x))
      ([], []) -> Nothing
      ([x], xs) -> Just (EThingWith (toQName x) (map toCName xs))
      x -> error $ "unexpected decl symbols: " ++ show x
    where
      toQName :: Symbol -> QName
      toQName x = UnQual (symbolName x)
      toCName :: Symbol -> CName
      toCName x@(Value {}) = varName (UnQual (symbolName x))
      toCName x@(Method {}) = varName (UnQual (symbolName x))
      toCName x@(Selector {}) = varName (UnQual (symbolName x))
      toCName x@(Constructor {}) = conName (UnQual (symbolName x))
      toCName x = error $ "toCName: " ++ show x

isThing :: Symbol -> Bool
isThing (Type {}) = True
isThing (Data {}) = True
isThing (NewType {}) = True
isThing (Class {}) = True
isThing _ = False

varName :: QName -> CName
varName (UnQual name) = VarName name
varName (Qual _mname name) = VarName name
varName x = error $ "varName: " ++ show x

conName :: QName -> CName
conName (UnQual name) = ConName name
conName (Qual _mname name) = ConName name
conName x = error $ "conName: " ++ show x
