{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Refactor.ModuleInfo
    ( ModuleInfo(..)
    , fullPathOfModuleInfo
    , getTopDeclSymbols'
    ) where

import Data.Generics (Data, Typeable)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax -- (Decl(TypeSig), Module(Module), ModuleHead(ModuleHead), Name)
import Language.Haskell.Names (Symbol(Value))
import Language.Haskell.Names.GetBound (getBound)
import Language.Haskell.Names.GlobalSymbolTable as Global (Table)
import Language.Haskell.Names.ModuleSymbols ( getTopDeclSymbols)
import Language.Haskell.Names.SyntaxUtils (dropAnn)
import Refactor.ModuleKey (moduleFullPath, ModuleKey, moduleName')
import Refactor.Utils (con, EZPrint(ezPrint), gFind)

data ModuleInfo l =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: Module l
               , _moduleComments :: [Comment]
               , _modulePath :: FilePath
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               , _moduleGlobals :: Global.Table
               } deriving (Data, Typeable, Functor, Show)

instance EZPrint (ModuleInfo l) where
    ezPrint (ModuleInfo {_module = Module _ (Just (ModuleHead _ n _ _)) _ _ _}) = prettyPrint n
    ezPrint (ModuleInfo {_module = Module _ Nothing _ _ _}) = "Main"
    ezPrint (ModuleInfo {_module = _}) = error "ezPrint: unexpected module"

fullPathOfModuleInfo :: ModuleInfo l -> FilePath
fullPathOfModuleInfo = moduleFullPath . _moduleKey

-- | Workaround pending pull request https://github.com/haskell-suite/haskell-names/pull/72
getTopDeclSymbols' :: ModuleInfo l -> Decl l -> [Symbol]
getTopDeclSymbols' i decl =
    getTopDeclSymbols (_moduleGlobals i) modulename (dropAnn decl) ++
    case decl of
      TypeSig _ names _ -> map (Value modulename) (map dropAnn names)
      _ -> []
    where
      modulename = (moduleName' (_moduleKey i))

instance (SrcInfo l, Typeable l, Data l) => EZPrint (ModuleInfo l, Decl l) where
    ezPrint (_, InstDecl _ _ r _) = ezPrint r
    ezPrint (_, SpliceDecl _ e) = ezPrint e
    ezPrint (i, d) =
        case gFind (getTopDeclSymbols' i d) :: [Name ()] of
          [] -> case (getBound (_moduleGlobals i) d) of
                  [] -> "(con=" ++ con d ++ ") " ++ show (getTopDeclSymbols' i d)
                  xs -> ezPrint xs
          ns -> ezPrint ns
