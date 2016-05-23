{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, OverloadedLists, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Symbols
    ( FoldDeclared(foldDeclared)
    , FoldMembers(foldMembers)
    , symbolsDeclaredBy
    , members
    , exports
    , imports
    ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Annotated.Simplify (sName)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ClassDecl(..), CName(..), ConDecl(..), Decl(..), DeclHead(..), ExportSpec(..), FieldDecl(..), GadtDecl(..), ImportSpec(..), InstHead(..), InstRule(..), Match(..), Name, Pat(..), PatField(..), QName(..), QualConDecl(..), RPat(..))
import qualified Language.Haskell.Exts.Syntax as S (CName(..), ExportSpec(..), ImportSpec(..), ModuleName(..), Name(..), QName(..))
import Utils (EZPrint(ezPrint))

-- | Do a fold over the names that are declared in a declaration (not
-- every name that appears, just the ones that the declaration is
-- causing to exist - what's the word for that?  Reify!)  The function
-- argument takes a Maybe because some declarations don't cause a
-- symbol to become bound - instances, for example.
class FoldDeclared a where
    foldDeclared :: forall r. (S.Name -> r -> r) -> r -> a -> r

instance FoldDeclared (A.CName a) where
    foldDeclared f r (A.VarName _ x) = f (sName x) r
    foldDeclared f r (A.ConName _ x) = f (sName x) r

instance FoldDeclared (A.Decl a) where
    foldDeclared f r (A.TypeDecl _ x _t) = foldDeclared f r x  -- type x = ...
    foldDeclared f r (A.TypeFamDecl _ x _k) = foldDeclared f r x -- data family x = ...
    foldDeclared f r (A.ClosedTypeFamDecl _ x _k _ts) = foldDeclared f r x -- data family x = ...
    foldDeclared f r0 (A.DataDecl _ _ _ x cs _) = foldl' (foldDeclared f) (foldDeclared f r0 x) cs -- data/newtype _ x = ...
    foldDeclared f r (A.GDataDecl _ _ _ x _ _ _) = foldDeclared f r x
    foldDeclared f r (A.DataFamDecl _ _ x _) = foldDeclared f r x
    foldDeclared _ r (A.TypeInsDecl _ _ _) = r -- type instance _ = ...
    foldDeclared _ r (A.DataInsDecl _ _ _ _ _) = r -- data instance _ = ...
    foldDeclared _ r (A.GDataInsDecl _ _ _ _ _ _) = r -- data or newtype instance, GADT style
    foldDeclared f r (A.ClassDecl _ _ x _ mxs) = foldl' (foldDeclared f) (foldDeclared f r x) (fromMaybe [] mxs)  -- class context => x | fundeps where decls
    foldDeclared _ r (A.InstDecl _ _ _ _) = r -- type class instance
    foldDeclared f r (A.DerivDecl _ _ x) = foldDeclared f r x
    foldDeclared _ r (A.InfixDecl _ _ _ _) = r -- fixity
    foldDeclared _ r (A.DefaultDecl _ _) = r -- default (type1, type2 ...)
    foldDeclared _ r (A.SpliceDecl _ _) = r  -- template haskell splice declaration
    foldDeclared f r (A.TypeSig _ xs _) = foldl' (foldDeclared f) r xs
    foldDeclared f r (A.FunBind _ xs) = foldl' (foldDeclared f) r xs
    foldDeclared f r (A.PatBind _ x _ _) = foldDeclared f r x
    foldDeclared _f _r (A.ForImp _ _ _ _ _ _) = error "Unimplemented FoldDeclared instance: ForImp"
    foldDeclared _ _r (A.ForExp _ _ _ _ _) = error "Unimplemented FoldDeclared instance: ForExp"
    foldDeclared _ r (A.RulePragmaDecl _ _) = r
    foldDeclared _ r (A.DeprPragmaDecl _ _) = r
    foldDeclared _ r (A.WarnPragmaDecl _ _) = r
    foldDeclared f r (A.InlineSig _ _ _ x) = foldDeclared f r x
    foldDeclared f r (A.InlineConlikeSig _ _ x) = foldDeclared f r x
    foldDeclared f r (A.SpecSig _ _ x _) = foldDeclared f r x
    foldDeclared f r (A.SpecInlineSig _ _ _ x _) = foldDeclared f r x
    foldDeclared f r (A.InstSig _ x) = foldDeclared f r x
    foldDeclared _ r (A.MinimalPragma _ _) = r
    foldDeclared _ r (A.AnnPragma _ _) = r
    foldDeclared _ _ (A.PatSynSig _ _ _ _ _ _) = error "fixme"
    foldDeclared _ _ (A.PatSyn _ _ _ _) = error "fixme"
    foldDeclared _ _ (A.RoleAnnotDecl _ _ _) = error "fixme"

instance FoldDeclared (A.DeclHead a) where
    foldDeclared f r (A.DHead _ x) = foldDeclared f r x
    foldDeclared f r (A.DHApp _ x _) = foldDeclared f r x
    foldDeclared f r (A.DHInfix _ _ x) = foldDeclared f r x
    foldDeclared f r (A.DHParen _ x) = foldDeclared f r x
instance FoldDeclared (A.ClassDecl a) where
    foldDeclared f r (A.ClsDecl _ x) = foldDeclared f r x       -- ordinary declaration
    foldDeclared f r (A.ClsDataFam _ _ x _) = foldDeclared f r x        -- declaration of an associated data type
    foldDeclared f r (A.ClsTyFam _ x _) = foldDeclared f r x    -- declaration of an associated type synonym
    foldDeclared _ r (A.ClsTyDef _ _ _) = r -- default choice for an associated type synonym
    foldDeclared f r (A.ClsDefSig _ x _) = foldDeclared f r x -- default signature
instance FoldDeclared (A.InstHead a) where
    foldDeclared f r (A.IHCon _ x) = foldDeclared f r x
    foldDeclared f r (A.IHApp _ x _) = foldDeclared f r x
    -- foldDeclared f r (A.IHead _ x _) = foldDeclared f r x
    foldDeclared f r (A.IHInfix _ _ x) = foldDeclared f r x
    foldDeclared f r (A.IHParen _ x) = foldDeclared f r x
instance FoldDeclared (A.InstRule a) where
    foldDeclared f r (A.IRule _ _ _ x) = foldDeclared f r x
    foldDeclared f r (A.IParen _ x) = foldDeclared f r x
instance FoldDeclared (A.Match a) where
    foldDeclared f r (A.Match _ x _ _ _) = foldDeclared f r x
    foldDeclared f r (A.InfixMatch _ _ x _ _ _) = foldDeclared f r x
-- Can you declare something with a qualified name?
instance FoldDeclared (A.QName a) where
    foldDeclared f r (A.Qual _ _ x) = foldDeclared f r x
    foldDeclared f r (A.UnQual _ x) = foldDeclared f r x
    foldDeclared _ r (A.Special _ _) = r
instance FoldDeclared (A.Pat a) where
    foldDeclared f r (A.PVar _ x) = foldDeclared f r x  -- variable
    foldDeclared _ r (A.PLit _ _ _) = r -- literal constant
    foldDeclared f r (A.PNPlusK _ x _) = foldDeclared f r x     -- n+k pattern
    foldDeclared f r (A.PInfixApp _ p1 _qn p2) = let r' = foldDeclared f r p1 in foldDeclared f r' p2   -- pattern with an infix data constructor
    foldDeclared f r (A.PApp _ _ ps) = foldl' (foldDeclared f) r ps      -- data constructor and argument patterns
    foldDeclared f r (A.PTuple _ _ ps) = foldl' (foldDeclared f) r ps    -- tuple pattern
    foldDeclared f r (A.PList _ ps) = foldl' (foldDeclared f) r ps       -- list pattern
    foldDeclared f r (A.PParen _ x) = foldDeclared f r x        -- parenthesized pattern
    foldDeclared f r (A.PRec _ _qn fs) = foldl' (foldDeclared f) r fs    -- labelled pattern, record style
    foldDeclared f r (A.PAsPat _ x y) = let r' = foldDeclared f r x in foldDeclared f r' y      -- @-pattern
    foldDeclared _ r (A.PWildCard _) = r        -- wildcard pattern: _
    foldDeclared f r (A.PIrrPat _ x) = foldDeclared f r x       -- irrefutable pattern: ~pat
    foldDeclared f r (A.PatTypeSig _ x _) = foldDeclared f r x  -- pattern with type signature
    foldDeclared f r (A.PViewPat _ _ x) = foldDeclared f r x    -- view patterns of the form (exp -> pat)
    foldDeclared f r (A.PRPat _ rps) = foldl' (foldDeclared f) r rps     -- regular list pattern
    foldDeclared _f _r (A.PXTag _ _xn _pxs _mp _ps) = error "Unimplemented FoldDeclared instance: PXTag"        -- XML element pattern
    foldDeclared _f _r (A.PXETag _ _xn _pxs _mp) = error "Unimplemented FoldDeclared instance: PXETag"  -- XML singleton element pattern
    foldDeclared _f _r (A.PXPcdata _ _s) = error "Unimplemented FoldDeclared instance: XPcdata" -- XML PCDATA pattern
    foldDeclared _f _r (A.PXPatTag _ _p) = error "Unimplemented FoldDeclared instance: PXPatTag"        -- XML embedded pattern
    foldDeclared _f _r (A.PXRPats _ _rps) = error "Unimplemented FoldDeclared instance: PXRPats"        -- XML regular list pattern
    foldDeclared _ r (A.PQuasiQuote _ _ _) = r  -- quasi quote pattern: [$name| string |]
    foldDeclared f r (A.PBangPat _ x) = foldDeclared f r x      -- strict (bang) pattern: f !x = ...
instance FoldDeclared (A.PatField a) where
    foldDeclared f r (A.PFieldPat _ _n x) = foldDeclared f r x  -- ordinary label-pattern pair
    foldDeclared f r (A.PFieldPun _ x) = foldDeclared f r x     -- record field pun
    foldDeclared _ r (A.PFieldWildcard _) = r
instance FoldDeclared (A.RPat a) where
    foldDeclared f r (A.RPOp _ x _) = foldDeclared f r x        -- operator pattern, e.g. pat*
    foldDeclared f r (A.RPEither _ x y) = let r' = foldDeclared f r x in foldDeclared f r' y    -- choice pattern, e.g. (1 | 2)
    foldDeclared f r (A.RPSeq _ xs) = foldl' (foldDeclared f) r xs       -- sequence pattern, e.g. (| 1, 2, 3 |)
    foldDeclared f r (A.RPGuard _ x _) = foldDeclared f r x     -- guarded pattern, e.g. (| p | p < 3 |)
    foldDeclared f r (A.RPCAs _ n x) = let r' = foldDeclared f r n in foldDeclared f r' x       -- non-linear variable binding, e.g. (foo@:(1 | 2))*
    foldDeclared f r (A.RPAs _ n x) = let r' = foldDeclared f r n in foldDeclared f r' x        -- linear variable binding, e.g. foo@(1 | 2)
    foldDeclared f r (A.RPParen _ x) = foldDeclared f r x       -- parenthesised pattern, e.g. (2*)
    foldDeclared f r (A.RPPat _ x) = foldDeclared f r x -- an ordinary pattern

instance FoldDeclared (A.Name l) where
    foldDeclared f r x = f (sName x) r

-- Something imported can be exported
instance FoldDeclared (A.ImportSpec l) where
    foldDeclared f r (A.IVar _ name) = foldDeclared f r name
    foldDeclared f r (A.IAbs _ _ name) = foldDeclared f r name
    foldDeclared f r (A.IThingAll _ name) = foldDeclared f r name
    foldDeclared f r (A.IThingWith _ tname wnames) = foldl' (foldDeclared f) (foldDeclared f r tname) wnames

-- This is really an implementation of foldMentioned
instance FoldDeclared (A.ExportSpec l) where
    foldDeclared f r (A.EVar _ name) = foldDeclared f r name
    foldDeclared f r (A.EAbs _ _ name) = foldDeclared f r name
    foldDeclared f r (A.EThingAll _ name) = foldDeclared f r name
    foldDeclared f r (A.EThingWith _ name _) = foldDeclared f r name
    foldDeclared _ r (A.EModuleContents _ _) = r -- This probably won't work correctly

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

-- The following instances of FoldDeclared are only called by the FoldMembers instances.  Hopefully.
instance FoldDeclared (A.QualConDecl l) where
    foldDeclared f r (A.QualConDecl _l _ _ x) = foldDeclared f r x

-- Constructors and field names
instance FoldDeclared (A.ConDecl l) where
    foldDeclared f r (A.ConDecl _ x _ts) = foldDeclared f r x   -- ordinary data constructor
    foldDeclared f r (A.InfixConDecl _ _t1 x _t2) = foldDeclared f r x  -- infix data constructor
    foldDeclared f r0 (A.RecDecl _ x fs) = foldl' (foldDeclared f) (foldDeclared f r0 x) fs   -- record constructor

instance FoldDeclared (A.FieldDecl l) where
    foldDeclared f r (A.FieldDecl _ xs _) = foldl' (foldDeclared f) r xs

instance FoldDeclared (A.GadtDecl l) where
    foldDeclared f r (A.GadtDecl _ x xs _) = let r' = foldDeclared f r x in maybe r' (foldl' (foldDeclared f) r') xs

instance EZPrint S.Name where
    ezPrint = prettyPrint

instance EZPrint (Maybe S.ModuleName) where
    ezPrint (Just x) = prettyPrint x
    ezPrint Nothing = "Main"

instance EZPrint (A.Decl l) where
    ezPrint d = ezPrint (foldDeclared (:) [] d)
