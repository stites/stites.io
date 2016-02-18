Just like how we have the subclassing of:

    Functor => Applicative => Monad

there is a similar, but not exact, mapping with Arrows:

    Profunctor => Category => Arrow

Profunctors are cool! Let's see some typeclasses:

    class Functor where
        fmap :: (a -> b) -> f a -> f b

    -- the dual of Functor?
    class Contra where
        contramap :: (a -> b) -> f b -> f a

    class ProFunctor where
        --| a combination of the above
        dimap :: (a -> b) -> (c -> d) -> p b c -> p  a  d
      --dimap :: contra   -> functor  -> p c f -> p  c' f'


