Multi-parameter Type Classes
=============================

If a single-parameter type class implies a set of types, then a multi-parameter
type class is a relationship between types.

If we want to have some kind of generic collection type, we would create a
typeclass like so:

    class Collection c where
      insert :: c -> e -> c
      member :: c -> e -> Bool

    instance Collection [a] where
      insert xs x = x:xs
      member = flip elem

However the `Collection` typeclass has no idea what `e` is. To correct this we
use a multi-parameter type clas:

    {-# LANGUAGE MultiParamTypeClasses #-}
    class Eq e => Collection c e where
      insert :: c -> e -> c
      member :: c -> e -> Bool

    {-# LANGUAGE FlexibleInstances #-} -- out of order, purely for example
    instance Eq a => Collection [a] a where
      insert = flip (:)
      member = flip elem

We can, furthermore, add constraints to these relationships. These come in the
form of a "functional dependency." The idea being that, if we want to ensure
that a type is an output type, we would do something like:

    class MultiTypeOp a b out | a b -> out where
      (\\) :: a -> b -> out

This way we can lock down the relationship between a, b, and out — guarding
ourselves against dumb users.


[0]: https://en.wikibooks.org/wiki/Haskell/Advanced_type_classes

