---
layout: post
title: Left is for errors by notational implementation
---

{{ page.title }}
================

A code snippet from earlier -- it's a little funny since I was talking to dan chao
who does a lot of scalaz, but no haskell, and we couldn't pinpoint exactly why Left
is used to pass errors. If we all wrote right-to-left I guess we'd be doing things
the other way around.

    module Sum where

    data Sum a b = First a | Second b deriving (Show, Eq)

    -- OH! _This_ is why Left is for Errors and Right is for values.
    -- By the implementation of typeclasses, operating on the right values while ignoring
    -- the left is the natural notation when you write out the instances!

    instance Functor (Sum a) where
      fmap f (First e) = First e
      fmap f (Second b) = Second (f b)

    instance Applicative (Sum a) where
      pure v = Second v
      (First e) <*> _ = First e
      _ <*> (First e) = First e
      (Second f) <*> (Second b) = Second (f b)

    instance Monad (Sum a) where
      return = pure
      (First e) >>= _ = First e
      (Second b) >>= f = f b

