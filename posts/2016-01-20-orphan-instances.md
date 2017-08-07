---
layout: post
title: Orphan Instances
---

Typeclasses have unique pairings of the class and the instance for a particular
type.

**An orphan instance is a big deal.** It's when you have an instance for a datatype
and a typeclass, but the instance is not in the same module as said datatype or
typeclass.

Why is this important? Well if you don't own the typeclass/datatype you can `newtype`
it! Even when you think you want an orphan instance so that you can have multiple
instances for the same type, you still actually want to use newtype. Here's an
example of what this means:

    -- Listy.hs
    module Listy where

    newtype Listy a =
        Listy [a]
        deriving(Eq, Show)

    -- ListyInstances.hs
    module ListyInstances where

    import Data.Monoid
    import Listy

    instance Monoid (Listy a) where
      mempty = Listy []
      mappend (Listy l) (Listy l') = Listy $ mappend l l'

