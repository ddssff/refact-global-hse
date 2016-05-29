{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, OverloadedLists, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Symbols
    ( FoldMembers(foldMembers)
    , symbolsDeclaredBy
    , members
    , exports
    , imports
    ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(ClassDecl, DataDecl, GDataDecl))
import qualified Language.Haskell.Exts.Syntax as S (CName(..), ExportSpec(..), ImportSpec(..), Name(..), QName(..))
import Tmp (FoldDeclared(foldDeclared))

-- Return the set of symbols appearing in a construct.  Some
-- constructs, such as instance declarations, declare no symbols, in
-- which case Nothing is returned.  Some declare more than one.
symbolsDeclaredBy :: FoldDeclared a => a -> [S.Name]
symbolsDeclaredBy = reverse . foldDeclared (:) mempty

members :: FoldMembers a => a -> [S.Name]
members = foldMembers (:) mempty

exports :: (FoldDeclared a, FoldMembers a) => a -> [S.ExportSpec]
exports x = case (symbolsDeclaredBy x, members x) of
              ([n], []) -> [S.EVar (S.UnQual n)]
              ([n], ms) -> [S.EThingWith (S.UnQual n) (Prelude.map S.VarName ms)]
              ([], []) -> []
              ([], _) -> error "exports: members with no top level name"
              (ns, []) -> Prelude.map (S.EVar . S.UnQual) ns
              y -> error $ "exports: multiple top level names and member names: " ++ show y

imports :: (FoldDeclared a, FoldMembers a) => a -> [S.ImportSpec]
imports x = case (symbolsDeclaredBy x, members x) of
              ([n], []) -> [S.IVar n]
              ([n], ms) -> [S.IThingWith n (Prelude.map S.VarName ms)]
              ([], []) -> []
              ([], _ms) -> error "exports: members with no top level name"
              (ns, []) -> Prelude.map S.IVar ns
              y -> error $ "imports: multiple top level names and member names: " ++ show y

-- | Fold over the declared members - e.g. the name and method names of a class
-- declaration, the name, constructor name, and field names of a data declaration.
class FoldMembers a where
    foldMembers :: forall r. (S.Name -> r -> r) -> r -> a -> r

instance FoldMembers (A.Decl a) where
    foldMembers f r0 (A.ClassDecl _ _ hd _ mxs) = foldl' (foldDeclared f) (foldDeclared f r0 hd) (fromMaybe [] mxs)  -- class context => x | fundeps where decls
    foldMembers f r (A.DataDecl _ _ _ _ xs _) = foldl' (foldDeclared f) r xs -- data/newtype _ x = ...
    foldMembers f r (A.GDataDecl _ _ _ _ _ xs _) = foldl' (foldDeclared f) r xs
    foldMembers _ r _ = r
