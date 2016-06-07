{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module MoveSpec(MoveSpec(MoveSpec)
    , applyMoveSpec
    , traceMoveSpec
    , moveDeclsByName
    , moveInstDecls
    , instClassPred
    , moveSpliceDecls
    , splicePred
    ) where

import Data.Generics (Data)
import Data.Set as Set (fromList, member)
import Debug.Trace (trace)
import qualified Language.Haskell.Exts.Annotated as A (Decl(InstDecl, SpliceDecl), Exp(SpliceExp), InstHead(..), InstRule(..), ModuleName(ModuleName), Name(Ident), QName, Splice(..), Type)
import qualified Language.Haskell.Exts.Syntax as S (Name(Ident, Symbol))
import Language.Haskell.Names (Symbol(symbolName))
import ModuleInfo (getTopDeclSymbols', ModuleInfo(..))
import ModuleKey (ModuleKey(ModuleKey, _moduleName))
import Utils (gFind, simplify)

-- | Specifies where to move each declaration of each module.  Given a
-- departure module key and a declaration, return an arrival module key.
newtype MoveSpec = MoveSpec (ModuleInfo () -> A.Decl () -> ModuleKey)

instance Monoid MoveSpec where
    mempty = MoveSpec $ \i _ -> _moduleKey i
    mappend (MoveSpec f) (MoveSpec g) = MoveSpec $
      \i d ->
        let k1 = f i d
            k2 = g i d in
        if k1 == _moduleKey i
        then k2
        else if k2 == _moduleKey i
             then k1
             else if k1 == k2
                  then k1
                  else error "Conflicting move specs"

applyMoveSpec :: Data l => MoveSpec -> ModuleInfo l -> A.Decl l -> ModuleKey
applyMoveSpec (MoveSpec f) i d = f (simplify i) (simplify d)

traceMoveSpec :: MoveSpec -> MoveSpec
traceMoveSpec (MoveSpec f) = MoveSpec $ \i d ->
  let k' = f i d in
  if _moduleKey i /= k'
  then (trace ("moveSpec " ++ show (_moduleKey i) ++ " d -> " ++ show k') k')
  else k'

-- A simple MoveSpec builder.
moveDeclsByName :: String -> String -> String -> MoveSpec
moveDeclsByName symname departMod arriveMod = MoveSpec $
    \i decl ->
        let syms = (Set.fromList . map symbolName) (getTopDeclSymbols' i decl) in
        case _moduleKey i of
          ModuleKey {_moduleName = A.ModuleName l name}
              | name == departMod && (Set.member (S.Ident symname) syms || Set.member (S.Symbol symname) syms) ->
                  (_moduleKey i) {_moduleName = A.ModuleName l arriveMod}
          _ -> _moduleKey i

moveInstDecls :: (ModuleInfo () -> A.QName () -> [A.Type ()] -> ModuleKey) -> MoveSpec
moveInstDecls instpred =
    MoveSpec f
    where
      f :: ModuleInfo () -> A.Decl () -> ModuleKey
      f i (A.InstDecl _ _ irule _) = g i irule
      f i _ = _moduleKey i
      g :: ModuleInfo () -> A.InstRule () -> ModuleKey
      g i (A.IParen _ irule) = g i irule
      g i (A.IRule _ _ _ ihead) = uncurry (instpred i) (h [] ihead)
      h :: [A.Type ()] -> A.InstHead () -> (A.QName (), [A.Type ()])
      h types (A.IHParen _ ihead) = h types ihead
      h types (A.IHApp _ ihead typ) = h (typ : types) ihead
      h types (A.IHCon _ name) = (name, types)
      h types (A.IHInfix _ typ name) = (name, typ : types)

moveSpliceDecls :: (ModuleInfo () -> A.Exp () -> ModuleKey) -> MoveSpec
moveSpliceDecls exppred =
    MoveSpec f
    where
      f :: ModuleInfo () -> A.Decl () -> ModuleKey
      f i (A.SpliceDecl _ exp') = g i exp'
      f i _ = _moduleKey i
      g :: ModuleInfo () -> A.Exp () -> ModuleKey
      g i (A.SpliceExp _ splice) = h i splice
      g i _ = _moduleKey i
      h :: ModuleInfo () -> A.Splice () -> ModuleKey
      h i (A.IdSplice _ _) = _moduleKey i
      h i (A.ParenSplice _ exp') = exppred i exp'

-- | Build the argument to moveInstDecls
instClassPred :: forall l. Data l => String -> String -> String ->
                 ModuleInfo l -> A.QName l -> [A.Type l] -> ModuleKey
instClassPred classname depart arrive (ModuleInfo {_moduleKey = key@ModuleKey {_moduleName = mname}}) qname _ts
    | simplify mname == A.ModuleName () depart &&
      (gFind (simplify qname) :: [A.Name ()]) == [A.Ident () classname] =
        key {_moduleName = A.ModuleName () arrive}
instClassPred _ _ _ i _ _ = _moduleKey i

-- | Build the argument to moveInstDecls
splicePred :: forall l. Data l => String -> String -> String ->
                 ModuleInfo l -> A.Exp l -> ModuleKey
splicePred name depart arrive (ModuleInfo {_moduleKey = key@ModuleKey {_moduleName = mname}}) exp'
    | simplify mname == A.ModuleName () depart &&
      elem (A.Ident () name) (gFind (simplify exp') :: [A.Name ()]) =
        key {_moduleName = A.ModuleName () arrive}
splicePred _ _ _ i _ = _moduleKey i
