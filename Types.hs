{-# LANGUAGE ScopedTypeVariables #-}

module Types
    ( ModuleKey(..)
    , ModuleInfo(..)
    , hseExtensions
    , hsFlags
    , hsSourceDirs
    , loadModule
    , DerivDeclTypes(derivDeclTypes)
    ) where

import qualified CPP (BoolOptions(locations), CpphsOptions(boolopts), defaultCpphsOptions, parseFileWithCommentsAndCPP)
import Control.Exception (Exception)
import Control.Exception.Lifted as IO (try)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Generics (everywhere, mkT)
import Data.List (groupBy, intercalate)
import Data.Set as Set (empty, Set, singleton, union, unions)
import qualified Language.Haskell.Exts.Annotated as A (Decl(DerivDecl), InstHead(..), InstRule(..), Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName), QName(Qual, UnQual), Type(..))
import Language.Haskell.Exts.Annotated.Simplify as S (sModuleName, sName)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(..))
import SrcLoc (fixSpan, textSpan)
import System.Directory (canonicalizePath)
import System.FilePath (addExtension, dropExtension, joinPath, splitDirectories, splitExtension, splitFileName)
import Text.PrettyPrint.HughesPJClass as PP (Pretty(pPrint), prettyShow, text)

-- A module is uniquely identitifed by its path and name
data ModuleKey =
    ModuleKey { _moduleTop :: FilePath      -- The <dir> for which ghc -i<dir> finds this module
              , _modulePath :: FilePath     -- The path relative to _moduleTop where the module text is
              , _moduleName :: S.ModuleName -- The module name
              } deriving (Eq, Ord, Show)
data ModuleInfo =
    ModuleInfo { _moduleKey :: ModuleKey
               , _module :: A.Module SrcSpanInfo
               , _moduleComments :: [Comment]
               , _moduleText :: String
               , _moduleSpan :: SrcSpanInfo
               }

  -- | From hsx2hs, but removing Arrows because it makes test case
-- fold3c and others fail.  Maybe we should parse the headers and then
-- use the options there?  There are functions to do this.
hseExtensions :: [Extension]
hseExtensions = map nameToExtension
    [ RecursiveDo, ParallelListComp, MultiParamTypeClasses, FunctionalDependencies, RankNTypes, ExistentialQuantification
    , ScopedTypeVariables, ImplicitParams, FlexibleContexts, FlexibleInstances, EmptyDataDecls, KindSignatures
    , BangPatterns, TemplateHaskell, ForeignFunctionInterface, {- Arrows, -} DeriveGeneric, NamedFieldPuns, PatternGuards
    , MagicHash, TypeFamilies, StandaloneDeriving, TypeOperators, RecordWildCards, GADTs, UnboxedTuples
    , PackageImports, QuasiQuotes, {-TransformListComp,-} ViewPatterns, {-XmlSyntax, RegularPatterns,-} TupleSections
    , ExplicitNamespaces
    ]
    where
      nameToExtension :: KnownExtension -> Extension
      nameToExtension x = EnableExtension x

hsFlags :: [String]
hsFlags = []

hsSourceDirs :: [FilePath]
hsSourceDirs = []

loadModule :: Exception e => FilePath -> IO (Either e ModuleInfo)
loadModule path =
  try loadModule' {- `IO.catch` (\(e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e) -}
    where
      loadModule' :: IO ModuleInfo
      loadModule' = do
        moduleText <- liftIO $ readFile path
        (parsed', comments, processed) <- Exts.fromParseResult <$> CPP.parseFileWithCommentsAndCPP cpphsOptions mode path
        let parsed = everywhere (mkT fixSpan) parsed'
        -- liftIO $ writeFile (path ++ ".cpp") processed
        -- putStr processed
        -- validateParseResults parsed comments processed -- moduleText
        key <- moduleKey path parsed
        putStrLn ("loaded " ++ prettyShow key)
        pure $ ModuleInfo { _moduleKey = key
                          , _module = parsed
                          , _moduleComments = comments
                          , _moduleText = moduleText
                          , _moduleSpan = textSpan path moduleText }
      mode = Exts.defaultParseMode {Exts.extensions = hseExtensions, Exts.parseFilename = path, Exts.fixities = Nothing }

-- | Turn of the locations flag.  This means simple #if macros will not
-- affect the line numbers of the output text, so we can use the
-- resulting SrcSpan info on the original text.  Macro expansions
-- could still mess this up.
cpphsOptions =
    CPP.defaultCpphsOptions
    { CPP.boolopts =
          (CPP.boolopts CPP.defaultCpphsOptions)
          { CPP.locations = False
      }
    }

-- | Compute the module key from a filepath and the parsed module.
moduleKey :: FilePath -> A.Module SrcSpanInfo -> IO ModuleKey
moduleKey _ (A.XmlPage {}) = error "XmlPage"
moduleKey _ (A.XmlHybrid {}) = error "XmlHybrid"
moduleKey path (A.Module _ Nothing _ _ _) = do
  path' <- canonicalizePath path
  let (top, sub) = splitFileName path'
      sub' = dropExtension sub
  pure $ ModuleKey top sub (S.ModuleName sub' {-"Main"-})
moduleKey path (A.Module _ (Just (A.ModuleHead _ (A.ModuleName _ name) _ _)) _ _ _) = do
  path' <- canonicalizePath path
  let name' = splitModuleName name
      (path'', ext) = splitExtension path'
      dirs = splitDirectories path''
      (dirs', name'') = splitAt (length dirs - length name') dirs
  when (name'' /= name') (error $ "Module name mismatch - name: " ++ show name' ++ ", path: " ++ show name'')
  pure $ ModuleKey (joinPath dirs')
                   (addExtension (joinPath name'') ext)
                   (S.ModuleName (intercalate "." name''))
      where
        splitModuleName = filter (/= ".") . groupBy (\a b -> (a /= '.') && (b /= '.'))

instance Pretty ModuleKey where
    pPrint (ModuleKey {_moduleTop = t, _moduleName = S.ModuleName n}) = text (n ++ " (in " ++ show t ++ ")")

-- | Collect the declared types of a standalone deriving declaration.
class DerivDeclTypes a where
    derivDeclTypes :: a -> Set (Maybe S.ModuleName, S.Name)

instance DerivDeclTypes (A.Decl l) where
    derivDeclTypes (A.DerivDecl _ _ x) = derivDeclTypes x
    derivDeclTypes _ = empty

instance DerivDeclTypes (A.InstRule l) where
    derivDeclTypes (A.IRule _ _ _ x)  = derivDeclTypes x
    derivDeclTypes (A.IParen _ x) = derivDeclTypes x

instance DerivDeclTypes (A.InstHead l) where
    derivDeclTypes (A.IHCon _ _) = empty
    derivDeclTypes (A.IHParen _ x) = derivDeclTypes x
    derivDeclTypes (A.IHInfix _ x _op) = derivDeclTypes x
    derivDeclTypes (A.IHApp _ x y) = union (derivDeclTypes x) (derivDeclTypes y)

instance DerivDeclTypes (A.Type l) where
    derivDeclTypes (A.TyForall _ _ _ x) = derivDeclTypes x -- qualified type
    derivDeclTypes (A.TyFun _ x y) = union (derivDeclTypes x) (derivDeclTypes y) -- function type
    derivDeclTypes (A.TyTuple _ _ xs) = unions (Prelude.map derivDeclTypes xs) -- tuple type, possibly boxed
    derivDeclTypes (A.TyList _ x) =  derivDeclTypes x -- list syntax, e.g. [a], as opposed to [] a
    derivDeclTypes (A.TyApp _ x y) = union (derivDeclTypes x) (derivDeclTypes y) -- application of a type constructor
    derivDeclTypes (A.TyVar _ _) = empty -- type variable
    derivDeclTypes (A.TyCon _ (A.Qual _ m n)) = singleton (Just (sModuleName m), sName n) -- named type or type constructor
       -- Unqualified names refer to imports without "qualified" or "as" values.
    derivDeclTypes (A.TyCon _ (A.UnQual _ n)) = singleton (Nothing, sName n)
    derivDeclTypes (A.TyCon _ _) = empty
    derivDeclTypes (A.TyParen _ x) = derivDeclTypes x -- type surrounded by parentheses
    derivDeclTypes (A.TyInfix _ x _op y) = union (derivDeclTypes x) (derivDeclTypes y) -- infix type constructor
    derivDeclTypes (A.TyKind _ x _) = derivDeclTypes x -- type with explicit kind signature
    derivDeclTypes (A.TyParArray _ x) = derivDeclTypes x
    derivDeclTypes (A.TyPromoted _ _) = empty
    derivDeclTypes (A.TyEquals _ _ _) = empty -- a ~ b, not clear how this related to standalone deriving
    derivDeclTypes (A.TySplice _ _) = empty
    derivDeclTypes (A.TyBang _ _ x) = derivDeclTypes x
    derivDeclTypes (A.TyWildCard _ _) = empty
