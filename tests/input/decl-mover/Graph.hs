{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Graph
    ( Rd(Rd, _modules, _environment)
    , MoveType(Down, Up)
    , makeImportGraph
    , findModuleByKey
    , findModuleByKeyUnsafe
    , moveType
    ) where

import Data.Foldable as Foldable (find)
import Data.Graph
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts.Syntax (ImportDecl(importModule), Module(Module), ModuleName)
import Language.Haskell.Names (Environment)
import Language.Haskell.Names.SyntaxUtils (dropAnn)
import ModuleInfo (ModuleInfo(ModuleInfo, _module, _moduleKey))
import ModuleKey (ModuleKey(..))


data Rd l
    = Rd { _modules :: [ModuleInfo l]
         , _environment :: Environment
         , _importGraph :: (Graph,
                            Vertex -> ((), ModuleName (), [ModuleName ()]),
                            ModuleName () -> Maybe Vertex) }

-- | Declaration moves can be characterized as one of two types, Down
-- or Up.  This must be computed by scanning the parsed code of the
-- departure module (the module where the declaration is when we
-- begin) for any remaining uses of the declaration's symbols.  Note
-- that it is possible to specify a move that results in a legitimate
-- import loop.  The only solution to this is to bring more
-- declarations over, or some manual intervention.
data MoveType
    = Down
    -- ^ A Down move moves a declaration away from where it is used,
    -- which means we probably need to add imports of the symbols of
    -- the declaration to the departure module.
    | Up
    -- ^ An Up move moves a declaration towards where it is used.  That
    -- means the arrive module already imports the depart module, though
    -- that import may disappear when the module is cleaned.  So we shouldn't
    -- add an import of the arrive module to the depart module as we would
    -- for a down move.
    deriving Show


-- | Build a graph of the "imports" relation.
makeImportGraph :: [ModuleInfo l] -> (Graph,
                                      Vertex -> ((), ModuleName (), [ModuleName ()]),
                                      ModuleName () -> Maybe Vertex)
makeImportGraph mods =
    graphFromEdges (mapMaybe (\m -> case _moduleKey m of
                                      ModuleKey {_moduleName = a} -> Just ((), a, modulesImportedBy m)
                                      _ -> Nothing) (map dropAnn mods))
    where
      -- What modules are imported by m?
      modulesImportedBy :: ModuleInfo l -> [ModuleName ()]
      modulesImportedBy (ModuleInfo {_module = Module _ _ _ imports _}) =
          nub $ map (dropAnn . importModule) imports
      modulesImportedBy _ = []

-- | Unsafe ModuleInfo lookup
findModuleByKey :: forall l. [ModuleInfo l] -> ModuleKey -> Maybe (ModuleInfo l)
findModuleByKey mods thisKey = Foldable.find (\m -> _moduleKey m == thisKey) mods

findModuleByKeyUnsafe :: forall l. [ModuleInfo l] -> ModuleKey -> ModuleInfo l
findModuleByKeyUnsafe mods thisKey = maybe (error $ "Module not found: " ++ show thisKey) id $ findModuleByKey mods thisKey

-- | If a declaration move requires us to import the departure module
-- into the arrival module it is called an "Up" move (towards where it
-- is used, away from the declarations it uses.)  When a declaration
-- move requires us to add an import of the declaration's symbols in
-- the departure module it is a "Down" move, away from where it is
-- used and towards the declarations that it uses.  Making both these
-- additions would create an import cycle, so we need to figure out
-- which it is:
--
--   (1) If the arrival module already imports the departure module it
--       must be an Up move.
--   (2) If the symbol is still used in the departure module by some
--       declaration that is not moving out, or by some declaration
--       that is moving in, it is a Down move.  This is a difficult
--       computation, so for now we assume it is true unless explicitly
--       marked "Up".
moveType :: Rd l -> ModuleKey -> ModuleKey -> MoveType
moveType (Rd _ _ (gr, _v2k, k2v)) (ModuleKey {_moduleName = depart}) (ModuleKey {_moduleName = arrive}) =
    case (k2v depart, k2v arrive) of
      (Just depart', Just arrive') ->
          -- If we move something from D to a module A that already
          -- imports D, that is an Up move.
          if depart' `elem` reachable gr arrive' then Up else Down
      _ -> Down -- A Down move is less likely to create an import cycle, so it is the default
moveType _ _ _ = Down
