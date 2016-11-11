-- Test moving s2 to a place that imports it
module M1
    ( s1
#if MIN_VERSION_base(4,8,0)
    , s2
#endif
    ) where

s1 :: Int
s1 = 1

#if MIN_VERSION_base(4,8,0)
s2 :: Int
s2 = s1 + 1
#endif
