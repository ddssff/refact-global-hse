module MoveSpec
    ( MoveSpec
    , appendMoveSpecs
    , identityMoveSpec
    , makeMoveSpec
    ) where

import Data.Set as Set (insert, member)
import qualified Language.Haskell.Exts.Annotated as A (Decl)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (Name(Ident, Symbol), ModuleName(ModuleName))
import Symbols (FoldDeclared(foldDeclared))
import Types (ModuleKey(_moduleName))

-- | Specifies where to move each declaration of each module.  Given a
-- departure module key and a declaration, return an arrival module key.
type MoveSpec = ModuleKey -> A.Decl SrcSpanInfo -> ModuleKey

appendMoveSpecs :: MoveSpec -> MoveSpec -> MoveSpec
appendMoveSpecs f g =
    \k0 d ->
        case (f k0 d, g k0 d) of
          (k1, k2) | k1 == k2 -> k1
          (k1, k2) | k1 == k0 -> k2
          _ -> k0

identityMoveSpec = \k _ -> k

-- A simple MoveSpec builder.
makeMoveSpec :: String -> String -> String -> MoveSpec
makeMoveSpec fname mname mname' =
    \mkey decl ->
        let syms = foldDeclared Set.insert mempty decl in
        if _moduleName mkey == Just (S.ModuleName mname) && (Set.member (S.Ident fname) syms || Set.member (S.Symbol fname) syms)
        then {-t1 mkey decl-} (mkey {_moduleName = Just (S.ModuleName mname')})
        else mkey
    -- where
      -- t1 mkey decl x = trace ("moveSpec " ++ show mkey ++ " " ++ show (foldDeclared (:) [] decl) ++ " -> " ++ show x) x
