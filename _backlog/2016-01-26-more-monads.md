(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m =>       m a -> m b -> m b

Thus:
    putStrLn "Hello, " >> putStrLn "world!"
is the same as:
    putStrLn "Hello, " *> putStrLn "world!"

Notice `putStrLn <$> getLine` only evaluates the IO for input, doesn't evaluate the print.

    :t fmap putStrLn getLine
    fmap putStrLn getLine :: IO (IO ())
    -- because
    :t getLine
    getLine :: IO String
    :t putStrLn
    putStrLn :: String -> IO ()
    :t fmap
    fmap :: Functor f => (a -> b) -> f a -> f b
    :t fmap putStrLn
    fmap putStrLn :: Functor f => f String -> f (IO ())





