module Utils where

-- | dropWhile where predicate operates on two list elements.
dropWhile2 :: (a -> Maybe a -> Bool) -> [a] -> [a]
dropWhile2 f (p : q : rs) | f p (Just q) = dropWhile2 f (q : rs)
dropWhile2 f [p] | f p Nothing = []
dropWhile2 f l = l
