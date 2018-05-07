module M2
    ( s3
    , s2
    ) where
import M1 (s1)
import M1

s3 :: Int
s3 = s2 + 1

s2 = s1 + 1
