module B(listPairs
    ) where




listPairs :: [a] -> [(a, Maybe a)]
listPairs (x1 : x2 : xs) = (x1, Just x2) : listPairs (x2 : xs)
listPairs [x] = [(x, Nothing)]
listPairs [] = []
