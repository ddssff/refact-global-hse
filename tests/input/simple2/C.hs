module C where

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
