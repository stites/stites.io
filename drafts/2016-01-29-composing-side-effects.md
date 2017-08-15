---
date: 1900-01-01
---


    ghci> putStrLn <$> getLine
    typing something in
    ghci> -- nothing was printed to console!

    h :: IO (IO ())
    h = putStrLn <$> getLine

It doesn't do what you'd think! So what _is_ it doing? Well, notice the two `IO`s;
they aren't actually referring to the same thing. The outtermost IO is in reference
to `getLine`'s IO which you need in order to retrieve the string to pipe into
the `putStrLn`'s IO, where you output to console. Let's break it down.

    :t putStrLn
    putStrLn :: String -> IO ()

    :t getLine
    getLine :: IO String

    :t (<$>)
    (<$>) :: Functor f => (a -> b) -> f a -> f b

    :t (putStrLn <$>)
    (putStrLn <$>) :: Functor f => f String -> f (IO ())

    :t (putStrLn <$> getLine)
    (putStrLn <$> getLine) :: IO (IO ())

Using our special sauce from Monad, `join`, we can then take these disjoint,
effectful IOs and compose the two to yield what we would expect:

    ghci> join $ putStrLn <$> getLine
    typing something in
    typing something in
    ghci> -- Much better!

    ghci> :t join $ putStrLn <$> getLine
    join $ putStrLn <$> getLine :: IO ()

From this we see how Haskell can merge the effects of getString and putStrLn into a
single action with an ordering of how these effects were nested.

