module MoveSpec(MoveSpec
    , appendMoveSpecs
    , identityMoveSpec
    ) where

import qualified Language.Haskell.Exts.Annotated as A (Decl)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Types (ModuleKey)


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
