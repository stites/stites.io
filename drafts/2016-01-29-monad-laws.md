Identity states that return should not perform computation.

    -- note
    (>>=) :: Monad m => m a -> (a -> m b) -> m b

    m >>= return   = m     -- right identity
    return m >>= f = f m   -- left identity

Associativity is also not any different from before, but looks different due to the
`>>=` semantics:

    -- normal-looking side   &    not-so-normal-looking side
       (m >>= f) >>= g       =    m >>= (\x -> f x >>= g)



