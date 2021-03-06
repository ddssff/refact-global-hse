{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}

module Refactor.Clean (cleanImports) where

import Control.Lens (over, view)
import Control.Monad (void)
import Control.Monad.RWS (MonadWriter(tell))
import Data.List (find, foldl1', intercalate, transpose)
import Data.Monoid ((<>))
import Data.Set as Set (empty, member, Set, singleton, unions)
import Debug.Trace (trace)
import Language.Haskell.Exts.Syntax (Decl(DerivDecl), ImportDecl(ImportDecl, importAs, importModule, importQualified, importSpecs),
  ImportSpec(IAbs, IThingAll, IThingWith, IVar), ImportSpecList(ImportSpecList), InstHead(..), InstRule(..),
  Module(Module), ModuleHead(ModuleHead), ModuleName(..), Name, QName(Qual, UnQual), Type(..))
import Language.Haskell.Exts.SrcLoc (SrcInfo, SrcSpanInfo)
import Language.Haskell.Names.SyntaxUtils (dropAnn, getImports, getModuleDecls)
import Refactor.CPP (cppEndif, cppIf, enabled, extensionsForHSEParser, GHCOpts, ghcProcessArgs, hc)
import Refactor.Imports (mergeDecls)
import Refactor.LoadModule (loadModule)
import Refactor.ModuleInfo (ModuleInfo(..))
import Refactor.ModuleKey (moduleFullPath)
import Refactor.ScanM (keep, keepAll, scanModule, skip, withTrailingWhitespace)
import Refactor.SrcLoc (EndLoc, endOfHeader, endOfImports, startOfDecls)
import Refactor.Utils (ezPrint, prettyPrint', replaceFile, SetLike(intersection, difference, union), withTempDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

-- | Replace the original imports of a module with cleaned imports as
-- produced by ghc with the @-ddump-minimal-imports@ flag.
cleanImports :: [GHCOpts] -> [ModuleInfo SrcSpanInfo] -> IO ()
cleanImports _ [] = trace ("cleanImports - no modules") (pure ())
cleanImports optSets mods = do
  imodSets <- mapM (doOpts mods) optSets :: IO [[(GHCOpts, [ImportDecl ()])]]
  mapM_ (uncurry doModule) (zip mods (transpose imodSets))

-- | Run ghc on the input files with the -ddump-minimal imports flag
doOpts :: [ModuleInfo SrcSpanInfo] -> GHCOpts -> IO [(GHCOpts, [ImportDecl ()])]
doOpts mods opts =
    withTempDirectory True "." "scratch" $ \scratch -> do
         hPutStrLn stderr ("cleanImports: " ++ ezPrint opts ++ " (scratch=" ++ scratch ++ ")")
         let args' = ["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch] ++
                     ghcProcessArgs (over enabled (++ extensionsForHSEParser) opts) ++
                     map _modulePath mods
         _out <- readProcess (view hc opts) args' ""
         map (opts,) <$> mapM (newImports opts scratch) mods

-- | Insert the cleaned and parsed imports into the parsed module,
-- rewrite files as necessary
doModule :: ModuleInfo SrcSpanInfo -> [(GHCOpts, [ImportDecl ()])] -> IO ()
doModule m pairs =
    do let newText = newModuleText m pairs
       let path = moduleFullPath (_moduleKey m)
       case newText of
         Nothing -> putStrLn (path <> " - imports already clean")
         Just s | _moduleText m /= s -> do
                      putStrLn (" imports changed")
                      void $ replaceFile path s
         Just _ -> pure ()

-- | Load the minimized imports output by ghc as a module.
newImports :: GHCOpts -> FilePath -> ModuleInfo SrcSpanInfo -> IO [ImportDecl ()]
newImports opts scratch (ModuleInfo {_module = Module _ mh _ _ _}) = do
  (\(ModuleInfo {_module = m}) -> fmap dropAnn (getImports m)) <$> loadModule opts importsPath
    where
      moduleName = maybe "Main" (\ (ModuleHead _ (ModuleName _ s) _ _) -> s) mh
      importsPath :: (Maybe FilePath, FilePath)
      importsPath = (Nothing, scratch </> moduleName ++ ".imports")
newImports _ _ _ = error "Unsupported module type"

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.  We also need to modify the imports of any names
-- that are types that appear in standalone instance derivations so
-- their members are imported too.
newModuleText :: forall l. (SrcInfo l, EndLoc l, Eq l) => ModuleInfo l -> [(GHCOpts, [ImportDecl ()])] -> Maybe String
newModuleText mi@(ModuleInfo {_module = m}) pairs =
    Just $ scanModule (do keep {-(startOfImports mi)-} (endOfHeader m)
                          let (common, pairs') = fixNewImports True mi pairs
                          tell "\n\n"
                          let ls :: [String]
                              ls = map prettyPrint' common ++ concatMap (uncurry doOptImports) pairs'
                          tell (intercalate "\n" ls)
                          -- Skip just past end of last original import
                          skip (endOfImports m)
                          -- Keep the comments between last import and first decl
                          -- keep (startOfDecls mi)
                          keepAll) mi
    where
      -- oi = getImports m
      doOptImports :: GHCOpts -> [ImportDecl ()] -> [String]
      doOptImports _opts [] = []
      doOptImports opts ni =
          -- let ni'' = fixNewImports' True mi ni in
          -- if dropAnn oi == map dropAnn ni' then keep ... else
          cppIf opts <>
          map prettyPrint' ni <>
          cppEndif opts

fixNewImports :: forall l. Bool -> ModuleInfo l -> [(GHCOpts, [ImportDecl ()])] -> ([ImportDecl ()], [(GHCOpts, [ImportDecl ()])])
fixNewImports _ _ [] = ([], [])
fixNewImports remove mi pairs =
    let pairs' = map (\(opts, ni) -> (opts, fixNewImports' remove mi ni)) pairs
        common = foldl1' intersection (map snd pairs')
        pairs'' = map (\(opts, imports) -> (opts, difference imports common)) pairs' in
    (common, pairs'')

-- | Final touch-ups - sort and merge similar imports.  (This code should be part of the SetLike instance)
fixNewImports' :: forall l.
                  Bool         -- ^ If true, imports that turn into empty lists will be removed
               -> ModuleInfo l
               -> [ImportDecl ()]
               -> [ImportDecl ()]
fixNewImports' remove mi@(ModuleInfo {_module = m}) ni =
    filter importPred $ map expandSDTypes $ mergeDecls $ ni ++ filter isHidingImport oi
    where
      oi = (fmap dropAnn (getImports m))
      expandSDTypes :: ImportDecl () -> ImportDecl ()
      expandSDTypes i@(ImportDecl {importSpecs = Just (ImportSpecList l f specs)}) =
          i {importSpecs = Just (ImportSpecList l f (Prelude.map (expandSpec i) specs))}
      expandSDTypes i = i
      expandSpec i s =
          if not (importQualified i) && member (Nothing, dropAnn n) sdTypes ||
             maybe False (\ mn -> (member (Just (dropAnn mn), dropAnn n) sdTypes)) (importAs i) ||
             member (Just (dropAnn (importModule i)), dropAnn n) sdTypes
          then s'
          else s
          where
            n = case s of
                  (IVar _ x) -> x
                  (IAbs _ _ x) -> x
                  (IThingAll _ x) -> x
                  (IThingWith _ x _) -> x
            s' = case s of
                  (IVar l x) -> IThingAll l x
                  (IAbs l _ x) -> IThingAll l x
                  (IThingWith l x _) -> IThingAll l x
                  (IThingAll _ _) -> s

      -- Eliminate imports that became empty
      -- importPred :: ImportDecl -> Bool
      importPred (ImportDecl _ mn _ _ _ _ _ (Just (ImportSpecList _ _ []))) =
          not remove || maybe False (isEmptyImport . importSpecs) (find ((== (dropAnn mn)) . importModule) oi)
          where
            isEmptyImport (Just (ImportSpecList _ _ [])) = True
            isEmptyImport _ = False
      importPred _ = True

      sdTypes :: Set (Maybe (ModuleName ()), Name ())
      sdTypes = standaloneDerivingTypes mi

isHidingImport :: ImportDecl l -> Bool
isHidingImport (ImportDecl {importSpecs = Just (ImportSpecList _ True _)}) = True
isHidingImport _ = False

standaloneDerivingTypes :: ModuleInfo l -> Set (Maybe (ModuleName ()), Name ())
standaloneDerivingTypes (ModuleInfo {_module = m}) = (unions . fmap derivDeclTypes . getModuleDecls) m

-- | Collect the declared types of a standalone deriving declaration.
class DerivDeclTypes a where
    derivDeclTypes :: a -> Set (Maybe (ModuleName ()), Name ())

instance DerivDeclTypes (Decl l) where
    derivDeclTypes (DerivDecl _ _ _ x) = derivDeclTypes x
    derivDeclTypes _ = empty

instance DerivDeclTypes (InstRule l) where
    derivDeclTypes (IRule _ _ _ x)  = derivDeclTypes x
    derivDeclTypes (IParen _ x) = derivDeclTypes x

instance DerivDeclTypes (InstHead l) where
    derivDeclTypes (IHCon _ _) = empty
    derivDeclTypes (IHParen _ x) = derivDeclTypes x
    derivDeclTypes (IHInfix _ x _op) = derivDeclTypes x
    derivDeclTypes (IHApp _ x y) = union (derivDeclTypes x) (derivDeclTypes y)

instance DerivDeclTypes (Type l) where
    derivDeclTypes (TyForall _ _ _ x) = derivDeclTypes x -- qualified type
    derivDeclTypes (TyFun _ x y) = union (derivDeclTypes x) (derivDeclTypes y) -- function type
    derivDeclTypes (TyTuple _ _ xs) = unions (Prelude.map derivDeclTypes xs) -- tuple type, possibly boxed
    derivDeclTypes (TyList _ x) =  derivDeclTypes x -- list syntax, e.g. [a], as opposed to [] a
    derivDeclTypes (TyApp _ x y) = union (derivDeclTypes x) (derivDeclTypes y) -- application of a type constructor
    derivDeclTypes (TyVar _ _) = empty -- type variable
    derivDeclTypes (TyCon _ (Qual _ m n)) = singleton (Just (dropAnn m), dropAnn n) -- named type or type constructor
       -- Unqualified names refer to imports without "qualified" or "as" values.
    derivDeclTypes (TyCon _ (UnQual _ n)) = singleton (Nothing, dropAnn n)
    derivDeclTypes (TyCon _ _) = empty
    derivDeclTypes (TyParen _ x) = derivDeclTypes x -- type surrounded by parentheses
    derivDeclTypes (TyInfix _ x _op y) = union (derivDeclTypes x) (derivDeclTypes y) -- infix type constructor
    derivDeclTypes (TyKind _ x _) = derivDeclTypes x -- type with explicit kind signature
    derivDeclTypes (TyParArray _ x) = derivDeclTypes x
    derivDeclTypes (TyPromoted _ _) = empty
    derivDeclTypes (TyEquals _ _ _) = empty -- a ~ b, not clear how this related to standalone deriving
    derivDeclTypes (TySplice _ _) = empty
    derivDeclTypes (TyBang _ _ _ x) = derivDeclTypes x
    derivDeclTypes (TyWildCard _ _) = empty
    derivDeclTypes (TyQuasiQuote _ _ _) = empty
