{-# LANGUAGE CPP #-}
module A where

import Data.String

#if 0
x = "This is inside #if 0"
#else
x = "This is inside the #else"
#endif

y = "This is outside the #if"
