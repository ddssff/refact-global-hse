{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Imports
    ( mergeDecls
    ) where

import Data.Char (toLower)
import Data.Default (Default(def))
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, nub, partition, sortBy)
import Data.Map.Strict as Map (adjust, elems, foldlWithKey', insertWith, Map)
import Data.Maybe (catMaybes, isNothing, maybeToList)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Syntax (ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec(..), ImportSpecList(..))
import Language.Haskell.Names.SyntaxUtils (dropAnn)
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

instance SetLike [ImportDecl ()] where
    union a b = mergeDecls (a <> b)
    difference a b =
        concat $  Map.elems $ foldlWithKey' f (importMap a) (importMap b)
         where
          f :: ImportMap () -> ImportKey -> [ImportDecl ()] -> ImportMap ()
          f a' k ads = adjust (maybeToList . specDifference k ads) k a'
          -- Compute the difference between two lists of mergable import declarations
          specDifference :: ImportKey -> [ImportDecl ()] -> [ImportDecl ()] -> Maybe (ImportDecl ())
          specDifference _ [] _ = Nothing
          specDifference (ImportDecl {}, hiding) a'@(a1 : _) b' =
              case specDifference' (concatMap specs a') (concatMap specs b') of
                [] -> Nothing
                xs -> Just (a1 {importSpecs = Just (ImportSpecList def hiding xs)})
          specDifference' :: [ImportSpec ()] -> [ImportSpec ()] -> [ImportSpec ()]
          specDifference' _ _ = [] -- undefined
          specs :: ImportDecl () -> [ImportSpec ()]
          specs i  = maybe [] (\(ImportSpecList _ _ ss) -> ss) (importSpecs i)

-- data ImportDecl l
--   = ImportDecl {importAnn :: l,
--                 importModule :: ModuleName l,
--                 importQualified :: Bool,
--                 importSrc :: Bool,
--                 importSafe :: Bool,
--                 importPkg :: Maybe String,
--                 importAs :: Maybe (ModuleName l),
--                 importSpecs :: Maybe (ImportSpecList l)}

importMap :: [ImportDecl ()] -> ImportMap ()
importMap xs = foldl' (\mp x -> Map.insertWith (<>) (importKey x) [x] mp) mempty xs

-- Build equivalence groups according to a function.
setify :: (a -> a -> Ordering) -> [a] -> [[a]]
setify f xs = groupBy (\a b -> f a b == EQ) . sortBy f $ xs

partitionMaybes :: [Maybe a] -> (Int, [a])
partitionMaybes xs = let (ns, js) = partition isNothing xs in (length ns, catMaybes js)

-- groupBy (\ a b -> importMergable a b == EQ) . sortBy importMergable

-- data ImportSpecList l = ImportSpecList l Bool [ImportSpec l]
--
-- data ImportSpec l
--     = IVar l (Name l)
--     | IAbs l (Namespace l) (Name l)
--     | IThingAll l (Name l)
--     | IThingWith l (Name l) [CName l]
--
-- data Namespace l = NoNamespace l | TypeNamespace l | PatternNamespace l

mergeDecls :: forall l. Eq l => [ImportDecl l] -> [ImportDecl l]
mergeDecls = map mergeDecls' . setify importMergable
    where
      -- Merge a group of decls that differ only in the spec list
      mergeDecls' :: [ImportDecl l] -> ImportDecl l
      mergeDecls' [] = error "mergeDecls"
      mergeDecls' xs@(x : _) = x {importSpecs = mergeSpecLists (fmap importSpecs xs)}

      -- Merge a list of specs for the same module.  A Nothing means
      -- all symbols are imported, Just [] means only instances are
      -- imported.
      mergeSpecLists :: [Maybe (ImportSpecList l)] -> Maybe (ImportSpecList l)
      mergeSpecLists xs =
          case partitionMaybes xs of
            (n, _) | n > 0 -> Nothing
            (_, []) -> error "groupBy failure"
            (_, ImportSpecList loc flag specs : more) ->
                Just (ImportSpecList loc flag (map (foldl1 mergeSpecs) (setify compareSpecs (specs ++ specLists more))))

      -- Merge several ImportSpecLists of matching ImportDecls
      specLists :: [ImportSpecList l] -> [ImportSpec l]
      specLists [] = []
      specLists (ImportSpecList _ _ specs : xs) = specs ++ specLists xs

      -- Compare function used to sort the symbols within an import,
      -- affects the order in which the results appear.
      compareSpecs :: ImportSpec l -> ImportSpec l -> Ordering
      compareSpecs a b =
          case (compare `on` everywhere (mkT (map toLower))) a' b' of
            EQ -> compare b' a' -- capitalized symbols should preceded lower case
            x -> x
          where
            a' = prettyPrint' a
            b' = prettyPrint' b

      -- Merge several import specs of the same symbol into one
      mergeSpecs :: ImportSpec l -> ImportSpec l -> ImportSpec l
      -- mergeSpecs x@(IAbs _ s n) _ = x
      -- mergeSpecs _ y@(IAbs _ s n) = y
      mergeSpecs x@(IThingAll _ _) _ = x
      mergeSpecs _ y@(IThingAll _ _) = y
      mergeSpecs (IThingWith l n ns) (IThingWith _ _ ms) = IThingWith l n (nub (ns ++ ms))
      -- mergeSpecs x@(IThingWith l n ns) (IAbs _ s _) = error "mergeSpecs: IThingWith + IAbs"
      mergeSpecs x@(IThingWith _l _n _ns) _ = x
      mergeSpecs x _ = x

      specSym (IVar _ n) = n
      specSym (IAbs _ _ n) = n -- Is this right?
      specSym (IThingAll _ n) = n
      specSym (IThingWith _ n _) = n

-- | Compare the two import declarations ignoring the things that are
-- actually being imported.  Equality here indicates that the two
-- imports could be merged.  Note that this also means the hiding flag
-- matches.
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
