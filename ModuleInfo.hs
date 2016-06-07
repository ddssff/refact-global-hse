{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}

module ModuleInfo
    ( ModuleInfo(..)
    , fullPathOfModuleInfo
    , getTopDeclSymbols'
    ) where

import Data.Generics (Data, Typeable)
import qualified Language.Haskell.Exts.Annotated as A -- (Decl(TypeSig), Module(Module), ModuleHead(ModuleHead), Name)
import Language.Haskell.Exts.Annotated.Simplify (sModuleName, sName, sType)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Names (Symbol(Value))
import Language.Haskell.Names.GetBound (getBound)
import Language.Haskell.Names.GlobalSymbolTable as Global (Table)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import ModuleKey (moduleFullPath, ModuleKey, moduleName')
import Utils (con, EZPrint(ezPrint), gFind, simplify)

data ModuleInfo l =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: A.Module l
               , _moduleComments :: [Comment]
               , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               , _moduleGlobals :: Global.Table
               } deriving (Data, Typeable, Functor)

instance EZPrint (ModuleInfo l) where
    ezPrint (ModuleInfo {_module = A.Module _ (Just (A.ModuleHead _ n _ _)) _ _ _}) = prettyPrint n
    ezPrint (ModuleInfo {_module = A.Module _ Nothing _ _ _}) = "Main"
    ezPrint (ModuleInfo {_module = _}) = error "ezPrint: unexpected module"

fullPathOfModuleInfo :: ModuleInfo l -> FilePath
fullPathOfModuleInfo = moduleFullPath . _moduleKey

-- | Workaround pending pull request https://github.com/haskell-suite/haskell-names/pull/72
getTopDeclSymbols' :: ModuleInfo l -> A.Decl l -> [Symbol]
getTopDeclSymbols' i decl =
    getTopDeclSymbols (_moduleGlobals i) modulename (simplify decl) ++
    case decl of
      A.TypeSig _ names _ -> map (Value (sModuleName modulename) . sName) names
      _ -> []
    where
      modulename = (moduleName' (_moduleKey i))

instance (A.SrcInfo l, Typeable l, Data l) => EZPrint (ModuleInfo l, A.Decl l) where
    ezPrint (i, A.InstDecl _ _ r _) = ezPrint r
    ezPrint (i, d) =
        case gFind (getTopDeclSymbols' i d) :: [S.Name] of
          [] -> case (getBound (_moduleGlobals i) d) of
                  [] -> "(con=" ++ con d ++ ") " ++ show (getTopDeclSymbols' i d)
                  xs -> ezPrint xs
          ns -> ezPrint ns

instance A.SrcInfo l => EZPrint (A.InstRule l) where
    ezPrint (A.IParen _ r) = ezPrint r
    ezPrint (A.IRule _ _ _ h) = "instance " ++ ezPrint h

instance A.SrcInfo l => EZPrint (A.InstHead l) where
    ezPrint (A.IHParen _ h) = ezPrint h
    ezPrint (A.IHInfix _ t n) = "(" ++ ezPrint n ++ ") " ++ ezPrint t
    ezPrint (A.IHCon _ n) = ezPrint n
    ezPrint (A.IHApp _ h t) = ezPrint h ++ " " ++ ezPrint t

instance EZPrint (A.QName l) where
    ezPrint = prettyPrint

instance EZPrint (A.Name l) where
    ezPrint = prettyPrint

instance A.SrcInfo l => EZPrint (A.Type l) where
    ezPrint = prettyPrint . sType
