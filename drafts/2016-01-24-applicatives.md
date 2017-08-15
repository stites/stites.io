---
date: 1900-01-01
---


applicative laws:

0. Typeclass:
  
  class Functor a => Applicative a where
    pure :: x -> a x
    pure val = app val
    (<*>) :: a (x -> y) -> a x -> a y
    (<*>) (app fn) (app x) = app (fn x)

I. Identity
    pure id <*> v = v

Keep in mind that we've built our vocabulary to be:

     $  ::   (a -> b) ->   a ->   b   -- function application
    <$> ::   (a -> b) -> f a -> f b   -- functor application
    <*> :: f (a -> b) -> f a -> f b   -- applicative application

         id     [1..5]   -- id [1,2,3,4,5]
    fmap id     [1..5]   -- [id 1, id 2, id 3, id 4, id 5]
    pure id <*> [1..5]   -- [id] <*> [1,2,3,4,5]  |-> [id 1, id 2, id 3, id 4, id 5]

II. Composition

    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

Which really means: `(u.v).w = u.(v.w)` for applicatives. So from CA:

    pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,3] = [(+1)] <*> ([(*2)] <*> [1,2,3])
                       [(+1).(*2)] <*> [1,2,3] = [(+1)] <*> [2,4,6]
                                       [3,5,7] = [3,5,7]

III. Homomorphism

"A homomorphism is a structure-preserving map between two categories"

    pure f <*> pure x = pure (f x)

basically, it should be identical to `(+1) 1`. Note that this law is basically
allowing us to say "forget the structure" but to such a degree that we will have to
give some kind of hint to the compiler as to what the structure is. In essence,
functions are evaluated to:

    pure (+1) <*> pure 1 :: Maybe Int
    pure ((+1) 1) :: Maybe Int

However, notice that:

    -- InvalidPure.hs
    module InvalidPure where
    nope = pure (+1) <*> pure 1

results in:

    ghci> :l InvalidPure.hs
    No instance for (Applicative f0) arising from a use of ‘<*>’
      The type variable ‘f0’ is ambiguous
      Relevant bindings include
        test :: f0 Integer (bound at /Users/stitess/InvalidPure.hs:3:1)
      Note: there are several potential instances:
        instance Monoid m => Applicative (Control.Applicative.Const m)
          -- Defined in ‘Control.Applicative’
        ...plus 15 others
      In the expression: pure (+ 1) <*> pure 1
      In an equation for ‘test’: test = pure (+ 1) <*> pure 1

Notice, however that the following works:

    ghci> let y = pure (+1) <*> pure 1
    ghci> :t y
    y :: (Num b, Applicative f) => f b

Because the Applicative in question is IO.

IV. Interchange

    u <*> pure y = pure ($ y) <*> u

because u is to the left of a tie-fighter, it must be a function embedded in some
structure. This means we can interpret as the following:

    -- on the left
    ghci> Just (+2) <*> pure 2
    Just 4

    -- on the right
    ghci> pure ($ 2) <*> Just (+2)
    Just 4

Notice that by sectioning the $ (function application) with y, we can "prime" a
parameter for some future operation. Notice:

    pure ($    y) <*>   u
       f (a -> b)     f a

Which translates to:

    pure ($ 2) <*> Just (+2)
    Just ($ 2) <*> Just (+2)
    Just (($ 2) (+2))
    Just ((+2) $ 2)
    Just (2 + 2)
    Just 4

More on the interchange law (have yet to review) from Catsters:

Natural Transformations 3: http://youtu.be/EG5xUYXHFeU
Natural Transformations 3A: http://youtu.be/fsfzEz6qAGQ

So remember that Applicatives must satisfy these four laws - identity, composition,
homomorphism, and interchange - and also satisfy the definition for Functor.






