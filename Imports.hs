{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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
import "haskell-src-exts-1ast" Language.Haskell.Exts.Syntax (Annotated(ann), ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec, ImportSpecList(..))
import "haskell-src-exts-1ast" Language.Haskell.Exts.SrcLoc (SrcLoc(SrcLoc), SrcInfo)
import Language.Haskell.Names.SyntaxUtils (dropAnn)
import SrcLoc (srcLoc)
import Utils (prettyPrint', SetLike(union, difference))

type ImportKey = (ImportDecl (), Bool)

-- | Return a value that can serve as a key to where and how the
-- import is importing, but not exactly what it is importing.  It just
-- throws away the specs and the SrcLoc info and records whether the
-- import was done with the "hiding" keyword.  This will distinguishes
-- "import Foo as F" from "import Foo", but will let us group imports
-- that can be merged.
importKey :: ImportDecl l -> ImportKey
importKey x = (cleanDecl, hiding)
    where
      cleanDecl = x' {importSpecs = Nothing}
      hiding = maybe False (\(ImportSpecList () f _) -> f) (importSpecs x')
      x' = fmap (const ()) x

type ImportMap l = Map ImportKey [ImportDecl l]

importMap :: [ImportDecl l] -> ImportMap l
importMap xs = foldl' (\mp x -> Map.insertWith (<>) (importKey x) [x] mp) mempty xs

mergeDecls :: forall l. Eq l => [ImportDecl l] -> [ImportDecl l]
mergeDecls = map mergeDecls' . groupBy (\ a b -> importMergable a b == EQ) . sortBy importMergable
    where
      mergeDecls' :: [ImportDecl l] -> ImportDecl l
      mergeDecls' [] = error "mergeDecls"
      mergeDecls' xs@(x : _) = x {importSpecs = mergeSpecLists (catMaybes (fmap importSpecs xs))}

      -- Merge a list of specs for the same module
      mergeSpecLists :: [ImportSpecList l] -> Maybe (ImportSpecList l)
      mergeSpecLists (ImportSpecList loc flag specs : ys) =
          Just (ImportSpecList loc flag (mergeSpecs (sortBy compareSpecs (nub (concat (specs : fmap (\ (ImportSpecList _ _ specs') -> specs') ys))))))
      mergeSpecLists [] = error "mergeSpecLists"

-- | Compare the two import declarations ignoring the things that are
-- actually being imported.  Equality here indicates that the two
-- imports could be merged.
importMergable :: ImportDecl l -> ImportDecl l -> Ordering
importMergable a b =
    case (compare `on` noSpecs) a' b' of
      EQ -> EQ
      specOrdering ->
          case (compare `on` importModule) a' b' of
            EQ -> specOrdering
            moduleNameOrdering -> moduleNameOrdering
    where
      a' = dropAnn a
      b' = dropAnn b
      -- Return a version of an ImportDecl with an empty spec list and no
      -- source locations.  This will distinguish "import Foo as F" from
      -- "import Foo", but will let us group imports that can be merged.
      -- Don't merge hiding imports with regular imports.
      noSpecs :: ImportDecl l -> ImportDecl l
      noSpecs x = x { importSpecs = case importSpecs x of
                                        Just (ImportSpecList l True _) -> Just (ImportSpecList l True []) -- hiding
                                        Just (ImportSpecList _ False _) -> Nothing
                                        Nothing -> Nothing }

-- Merge elements of a sorted spec list as possible
-- unimplemented, should merge Foo and Foo(..) into Foo(..), and the like
mergeSpecs :: [ImportSpec l] -> [ImportSpec l]
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
compareSpecs :: ImportSpec l -> ImportSpec l -> Ordering
-- compareSpecs a b = (compare `on` sImportSpec) a b
compareSpecs a b =
    case (compare `on` (everywhere (mkT (map toLower)))) a' b' of
      EQ -> compare b' a' -- upper case first
      x -> x
    where
      a' = prettyPrint' a
      b' = prettyPrint' b
