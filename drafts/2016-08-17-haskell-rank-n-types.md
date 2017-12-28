---
date: 1900-01-01
---


`forall a` is implicit for any function which is polymorphic:

    foo :: a -> Bool
    -- is actually
    foo :: forall a. a -> Bool

    -- or, for polymorphism on multiple types
    bar :: a -> b -> Bool
    bar :: forall a. forall b. a -> b -> Bool
    bar :: forall a b. a -> b -> Bool

when we explicitly state `forall`, we can specify the level of polymorphism
within a type signature:

    foo :: (forall a. a -> a) -> (Char, Bool)
    foo f = (f 'c', f True)

This is very different from a simple

    foo :: a -> a -> (Char, Bool)
    -- which expands to:
    foo :: forall a. (a -> a -> (Char, Bool))

in the latter example, `a` will be locked down too early and it will be
impossible to apply the `a -> a` to both `Char` and `Bool`. This is not a
problem in the former example.

This level of specificity makes the type signature have a rank-2 type. We can
generalize this to find types of rank-n.

