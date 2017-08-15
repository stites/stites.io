Monoid -> Semigroup

Semigroups are Monoids without ids. Core operations are binary and associative

    class Semigroup a where
      (<>) :: a -> a -> a

    -- with the law:
    -- (a <> b) <> c = a <> (b <> c)

Semigroup still probides a binary associative opreation that joins things together.
At this moment it's not a part of base.

NonEmpty can't have a Monoid instance, but can have a semigroup one. For instance if
you have a NonEmpty List, it can't return an empty list as an identity. This looks
like:

    data NonEmpty a = a :| [a]
      deriving (Eq, Ord, Show)
    -- other instances elided

Note: data constructors which _only_ non-alphnumeric symbols, _and_ that start with a
colon, are infix by default.

Monoid are _stronger_ than Semigroup. Eventually, Monoid will be superclassed by Semigroup:

    class Semigroup a => Monoid a where
      ...

So it's easy to find a semigroup! just take a monoid and remove the identity. That
means:
+ (+) -> increment
+ (*) -> multiply

A Magma is the super of semigroup. A Magma removes the associativity requirement.

In fact, we can sum it all up in this nice picture from wikipedia:
https://en.wikipedia.org/wiki/File:Magma_to_group2.svg

In fact! https://en.wikipedia.org/wiki/Magma_(algebra) is pretty slick too!


