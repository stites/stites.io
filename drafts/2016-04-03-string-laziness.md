---
date: 1900-01-01
---


Something very simple should get lazily evaluated, correct?

    Prelude> let myList = [1, 2, 3] :: [Integer]
    Prelude> :sprint myList
    myList = [1,2,3]

no! but if you take off the type signature:

    Prelude> let myList = [1, 2, 3]
    Prelude> :sprint myList
    myList = _
    Prelude> :t myList
    myList :: Num a => [a]

So the generifying `Num` will stop computation. Also:

    Prelude> let myList = [1, 2, id 1] :: [Integer]
    Prelude> :sprint myList
    myList = [1,2,_]

GHC opportunistically stops computation when it reaches a function.

But I think the cool part is really when we get to Strings

    Prelude> let a = Just ['a']
    Prelude> :sprint a
    a = Just "a"
    Prelude> let a = Just "a"
    Prelude> :sprint a
    a = Just _

What happened? Well strings are understood to be the same as character arrays
in haskell, but under the hood GHC is actually `unpacking` them as a second-
stage computation!


