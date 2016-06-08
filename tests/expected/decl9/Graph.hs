{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}
module Graph
    ( Rd(Rd, _modules, _environment)
    , MoveType(..)
    , findModuleByKey
    , findModuleByKeyUnsafe
    , moveType'
    , moveType
    , importsSymbolsFrom
    ) where

import Control.Exception (SomeException)
import Control.Lens (makeLenses, view)
import Control.Monad (foldM, void, when)
import Control.Monad.RWS (modify, MonadWriter(tell))
import Control.Monad.State (execState, MonadState)
import Data.Default (def)
import Data.Foldable as Foldable (find)
import Data.Generics (Data)
import Data.List ((\\), foldl', intercalate, nub, stripPrefix)
import Data.Map as Map (insertWith, Map, mapWithKey, toList)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybeToList)
import Data.Set as Set (fromList, insert, isSubsetOf, member, Set, toList)
import Debug.Trace (trace)
import GHC (GHCOpts(hsSourceDirs))
import Imports (cleanImports)
import qualified Language.Haskell.Exts.Annotated as A (Annotated(ann), Decl(TypeSig), ExportSpec, ExportSpecList(ExportSpecList), ImportDecl(importModule, importSpecs), ImportSpec, ImportSpecList(ImportSpecList), Module(Module), ModuleHead(ModuleHead), ModuleName(..), ModulePragma, Name, SrcInfo)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec, sModuleName, sModulePragma, sName)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(..), ImportDecl(..), ImportSpec(IThingAll, IThingWith, IVar), ModuleName(..), ModulePragma(..), Name(..), QName(Qual, Special, UnQual))
import Language.Haskell.Names (Environment, resolve, symbolName)
import LoadModule (Annot, loadModule, loadModules)
import ModuleInfo (getTopDeclSymbols', ModuleInfo(..))
import ModuleKey (moduleFullPath, ModuleKey(..), moduleName)
import MoveSpec (applyMoveSpec, MoveSpec)
import Names (topDeclExportSpec)
import SrcLoc (EndLoc(endLoc), endOfHeader, endOfImports, keep, keepAll, ScanM, scanModule, skip, srcLoc, startOfImports, withTrailingWhitespace)
import System.FilePath.Find as FilePath ((&&?), (==?), always, extension, fileType, FileType(RegularFile), find)
import Utils (EZPrint(ezPrint), gFind, listPairs, prettyPrint', replaceFile, simplify, withCleanRepo, withCurrentDirectory, withTempDirectory)

data Rd l
    = Rd { _modules :: [ModuleInfo l]
         , _environment :: Environment }

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
    -- ^ An Up move moves a declaration towards where it is used.  In
    -- this case leaving behind an import will probably create an
    -- import cycle.  Therefore we need to convert the (remaining)
    -- exports of the departure module into imports and add them to
    -- the arrival module.
    deriving Show

-- | Unsafe ModuleInfo lookup
findModuleByKey :: forall l. [ModuleInfo l] -> ModuleKey -> Maybe (ModuleInfo l)
findModuleByKey mods thisKey = Foldable.find (\m -> _moduleKey m == thisKey) mods

findModuleByKeyUnsafe :: forall l. [ModuleInfo l] -> ModuleKey -> ModuleInfo l
findModuleByKeyUnsafe mods thisKey = maybe (error $ "Module not found: " ++ show thisKey) id $ findModuleByKey mods thisKey

moveType' :: Rd l -> ModuleKey -> ModuleKey -> String
moveType' (Rd mods _env) thisKey' someKey' =
    case (findModuleByKey mods someKey', moduleName thisKey') of
      (Just (ModuleInfo {_module = A.Module _ _ _ someModuleImports _}), Just thisModuleName') ->
          show (moveType someModuleImports thisModuleName')
      _ -> "Unknown"

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

moveType :: [A.ImportDecl l] -> A.ModuleName () -> MoveType
moveType arrivalModuleImports departureModuleName =
    case arrivalModuleImports `importsSymbolsFrom` departureModuleName of
      True -> Up
      False -> Down

-- | Does module m import from name?
importsSymbolsFrom :: [A.ImportDecl l] -> A.ModuleName () -> Bool
importsSymbolsFrom imports importee = any (\i -> simplify (A.importModule i) == importee) imports
