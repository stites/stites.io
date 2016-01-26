
liftM and liftA are the same! liftM is around as a function to maintain compatability
with older libraries that used monads before applicatives existed.

zipWith is liftA2/liftM2 specialized with lists:

    ghci> :t liftM2
    liftM2 :: Monad f => (a -> b -> c) -> f a -> f b -> f c
    ghci> :t zipWith
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    ghci> zipWith (+) [3, 4] [5, 6]
    [8,10]
    ghci> liftA2 (+) [3, 4] [5, 6]
    [8,9,9,10]

    ghci> :t liftM3
    liftM3 :: Monad f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    ghci> :t zipWith3
    zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
    ghci> zipWith3 (,,) [1, 2] [3] [5, 6]
    [(1,3,5)]
    ghci> liftA3 (,,) [1, 2] [3] [5, 6]
    [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]


