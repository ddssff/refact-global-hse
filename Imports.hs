{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Imports
    ( mergeDecls
    ) where

import Data.Char (toLower)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, nub, sortBy)
import Data.Map.Strict as Map (adjust, elems, foldlWithKey', insertWith, Map)
import Data.Maybe (catMaybes, maybeToList)
import Data.Monoid ((<>))
import qualified Language.Haskell.Exts.Annotated as A (Annotated(ann), ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec, ImportSpecList(..), SrcLoc(SrcLoc))
import Language.Haskell.Exts.SrcLoc (SrcInfo)
import SrcLoc (srcLoc)
import Utils (prettyPrint', SetLike(union, difference), simplify)

type ImportKey = (A.ImportDecl (), Bool)

-- | Return a value that can serve as a key to where and how the
-- import is importing, but not exactly what it is importing.  It just
-- throws away the specs and the SrcLoc info and records whether the
-- import was done with the "hiding" keyword.  This will distinguishes
-- "import Foo as F" from "import Foo", but will let us group imports
-- that can be merged.
importKey :: A.ImportDecl l -> ImportKey
importKey x = (cleanDecl, hiding)
    where
      cleanDecl = x' {A.importSpecs = Nothing}
      hiding = maybe False (\(A.ImportSpecList () f _) -> f) (A.importSpecs x')
      x' = fmap (const ()) x

type ImportMap l = Map ImportKey [A.ImportDecl l]

importMap :: [A.ImportDecl l] -> ImportMap l
importMap xs = foldl' (\mp x -> Map.insertWith (<>) (importKey x) [x] mp) mempty xs

mergeDecls :: forall l. (SrcInfo l, Eq l) => [A.ImportDecl l] -> [A.ImportDecl l]
mergeDecls = map mergeDecls' . groupBy (\ a b -> importMergable a b == EQ) . sortBy importMergable
    where
      mergeDecls' :: [A.ImportDecl l] -> A.ImportDecl l
      mergeDecls' [] = error "mergeDecls"
      mergeDecls' xs@(x : _) = x {A.importSpecs = mergeSpecLists (catMaybes (Prelude.map A.importSpecs xs))}

      -- Merge a list of specs for the same module
      mergeSpecLists :: [A.ImportSpecList l] -> Maybe (A.ImportSpecList l)
      mergeSpecLists (A.ImportSpecList loc flag specs : ys) =
          Just (A.ImportSpecList loc flag (mergeSpecs (sortBy compareSpecs (nub (concat (specs : Prelude.map (\ (A.ImportSpecList _ _ specs') -> specs') ys))))))
      mergeSpecLists [] = error "mergeSpecLists"

-- | Compare the two import declarations ignoring the things that are
-- actually being imported.  Equality here indicates that the two
-- imports could be merged.
importMergable :: SrcInfo l => A.ImportDecl l -> A.ImportDecl l -> Ordering
importMergable a b =
    case (compare `on` noSpecs) a' b' of
      EQ -> EQ
      specOrdering ->
          case (compare `on` (A.importModule . simplify)) a' b' of
            EQ -> specOrdering
            moduleNameOrdering -> moduleNameOrdering
    where
      a' = simplify a
      b' = simplify b
      -- Return a version of an ImportDecl with an empty spec list and no
      -- source locations.  This will distinguish "import Foo as F" from
      -- "import Foo", but will let us group imports that can be merged.
      -- Don't merge hiding imports with regular imports.
      A.SrcLoc _path _ _ = srcLoc (A.ann a)
      noSpecs :: A.ImportDecl l -> A.ImportDecl l
      noSpecs x = x { A.importSpecs = case A.importSpecs x of
                                        Just (A.ImportSpecList l True _) -> Just (A.ImportSpecList l True []) -- hiding
                                        Just (A.ImportSpecList _ False _) -> Nothing
                                        Nothing -> Nothing }

-- Merge elements of a sorted spec list as possible
-- unimplemented, should merge Foo and Foo(..) into Foo(..), and the like
mergeSpecs :: [A.ImportSpec l] -> [A.ImportSpec l]
mergeSpecs [] = []
mergeSpecs [x] = [x]
{-
-- We need to do this using the simplified syntax
mergeSpecs (x : y : zs) =
    case (name x' == name y', x, y) of
      (True, S.IThingAll _ _, _) -> mergeSpecs (x : zs)
      (True, _, S.IThingAll _ _) -> mergeSpecs (y : zs)
      (True, S.IThingWith _ n xs, S.IThingWith _ ys) -> mergeSpecs (S.IThingWith n (nub (xs ++ ys)))
      (True, S.IThingWith _ _, _) -> mergeSpecs (x' : zs)
      (True, _, S.IThingWith _ _) -> mergeSpecs (y' : zs)
      _ -> x : mergeSpecs (y : zs)
    where
      x' = sImportSpec x
      y' = sImportSpec y
      name (S.IVar n) = n
      name (S.IAbs n) = n
      name (S.IThingAll n) = n
      name (S.IThingWith n _) = n
-}
mergeSpecs xs = xs

-- Compare function used to sort the symbols within an import.
compareSpecs :: A.ImportSpec l -> A.ImportSpec l -> Ordering
-- compareSpecs a b = (compare `on` sImportSpec) a b
compareSpecs a b =
    case (compare `on` (everywhere (mkT (map toLower)))) a' b' of
      EQ -> compare b' a' -- upper case first
      x -> x
    where
      a' = prettyPrint' a
      b' = prettyPrint' b
