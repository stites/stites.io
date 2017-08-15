get back your monad from a monad transformer:

    ghci> runMaybeT $ (+1) <$> MaybeT $ Identity (Just 1)
    Identity {runIdentity = Just 2}

