{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Refactor.MoveSpec
    ( MoveSpec(MoveSpec)
    , applyMoveSpec
    , traceMoveSpec
    , moveDeclsByName
    , moveInstDecls
    , instClassPred
    , moveSpliceDecls
    , splicePred
    ) where

import Data.Generics (Data)
import Data.Set as Set (map, member)
import Debug.Trace (trace)
import Language.Haskell.Exts.Syntax (Decl(InstDecl, SpliceDecl), Exp(SpliceExp), InstHead(..), InstRule(..), ModuleName(ModuleName), Name(Ident, Symbol), QName, Splice(..), Type)
import Language.Haskell.Names (Symbol(symbolName))
import Language.Haskell.Names.SyntaxUtils (dropAnn)
import Refactor.ModuleInfo (getTopSymbols, ModuleInfo(..))
import Refactor.ModuleKey (ModuleKey(ModuleKey, _moduleName))
import Refactor.Utils (gFind)

-- | Specifies where to move each declaration of each module.  Given a
-- departure module key and a declaration, return an arrival module key.
newtype MoveSpec = MoveSpec (ModuleInfo () -> Decl () -> ModuleKey)

instance Show MoveSpec where
    show _ = "<MoveSpec>"

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

applyMoveSpec :: Data l => MoveSpec -> ModuleInfo l -> Decl l -> ModuleKey
applyMoveSpec (MoveSpec f) i d = f (dropAnn i) (dropAnn d)

-- | Print a description of the move spec on the console.
traceMoveSpec :: MoveSpec -> MoveSpec
traceMoveSpec (MoveSpec f) = MoveSpec $ \i d ->
  let k' = f i d in
  if _moduleKey i /= k'
  then (trace ("moveSpec " ++ show (_moduleKey i) ++ " d -> " ++ show k') k')
  else k'

-- | A MoveSpec builder that selects declarations by symbol name.
moveDeclsByName :: String -> String -> String -> MoveSpec
moveDeclsByName symname departMod arriveMod = MoveSpec $
    \i decl ->
        let syms = Set.map symbolName (getTopSymbols i decl) in
        case _moduleKey i of
          ModuleKey {_moduleName = ModuleName l name}
              | name == departMod && (Set.member (Ident () symname) syms || Set.member (Symbol () symname) syms) ->
                  (_moduleKey i) {_moduleName = ModuleName l arriveMod}
          _ -> _moduleKey i

-- | A MoveSpec builder for instances (which have no name).
moveInstDecls :: (ModuleInfo () -> QName () -> [Type ()] -> ModuleKey) -> MoveSpec
moveInstDecls instpred =
    MoveSpec f
    where
      f :: ModuleInfo () -> Decl () -> ModuleKey
      f i (InstDecl _ _ irule _) = g i irule
      f i _ = _moduleKey i
      g :: ModuleInfo () -> InstRule () -> ModuleKey
      g i (IParen _ irule) = g i irule
      g i (IRule _ _ _ ihead) = uncurry (instpred i) (h [] ihead)
      h :: [Type ()] -> InstHead () -> (QName (), [Type ()])
      h types (IHParen _ ihead) = h types ihead
      h types (IHApp _ ihead typ) = h (typ : types) ihead
      h types (IHCon _ name) = (name, types)
      h types (IHInfix _ typ name) = (name, typ : types)

-- | Build a predicate to pass to 'moveInstDecls'
instClassPred :: forall l. Data l => String -> String -> String ->
                 ModuleInfo l -> QName l -> [Type l] -> ModuleKey
instClassPred classname depart arrive (ModuleInfo {_moduleKey = key@ModuleKey {_moduleName = mname}}) qname _ts
    | dropAnn mname == ModuleName () depart &&
      (gFind (dropAnn qname) :: [Name ()]) == [Ident () classname] =
        key {_moduleName = ModuleName () arrive}
instClassPred _ _ _ i _ _ = _moduleKey i

-- | A MoveSpec builder that selects template haskell splice declarations by predicate.
moveSpliceDecls :: (ModuleInfo () -> Exp () -> ModuleKey) -> MoveSpec
moveSpliceDecls exppred =
    MoveSpec f
    where
      f :: ModuleInfo () -> Decl () -> ModuleKey
      f i (SpliceDecl _ exp') = g i exp'
      f i _ = _moduleKey i
      g :: ModuleInfo () -> Exp () -> ModuleKey
      g i (SpliceExp _ splice) = h i splice
      g i _ = _moduleKey i
      h :: ModuleInfo () -> Splice () -> ModuleKey
      h i (IdSplice _ _) = _moduleKey i
      h i (ParenSplice _ exp') = exppred i exp'

-- | Build the argument to 'moveSpliceDecls'
splicePred :: forall l. Data l => String -> String -> String ->
                 ModuleInfo l -> Exp l -> ModuleKey
splicePred name depart arrive (ModuleInfo {_moduleKey = key@ModuleKey {_moduleName = mname}}) exp'
    | dropAnn mname == ModuleName () depart &&
      elem (Ident () name) (gFind (dropAnn exp') :: [Name ()]) =
        key {_moduleName = ModuleName () arrive}
splicePred _ _ _ i _ = _moduleKey i
