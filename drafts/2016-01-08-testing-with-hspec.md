---
date: 1900-01-01
---


---
layout: post
title: Testing with HSpec
---

{{ page.title }}
================

<p class="meta">08 Jan 2016 - Belmont, CA</p>

Notes from Haskell book:
+ unit testing - standard xUnit things
+ property testing - quickcheck
+ spec testing - like TDD? the book's description is lacking. Perhaps you should
  email Chris about this.
+ HUnit and HSpec test atomic units of code independently (?) seems like a JUnit
  situation so I don't know why you wouldn't do full e2e's

Property Testing
----------------
Pioneered in Haskell (source?). Basically, tests for formal properties without formal
proofs. Properties are required to be universally quantifiable/justifiable.

Thought: quickcheck for API integration tests in other languages. Mostly this is for
testing valid inputs and malformed json. This is very close to my original idea in
2014 of probabalistic testing which also included playback and reporting.


Property Generators
-------------------

Keep in mind that we can change the odds of a generator by changing the frequency of
a set. So instead of:

    oneThroughThree :: Gen Int
    oneThroughThree = elements [1, 2, 3]

Use:

    oneThroughThree :: Gen Int
    oneThroughThree = elements [1, 2, 2, 2, 2, 3]

We can use `choose`, `elements`, `return`, or `arbitrary` -- to name a few. With arbitrary, we can get powerful queues like:

    -- equal probability
    genMaybe :: Arbitrary a => Gen (Maybe a) genMaybe = do
      a <- arbitrary
      elements [Nothing, Just a]

    -- What QuickCheck actually does
    -- so you get more Just values
    genMaybe' :: Arbitrary a => Gen (Maybe a) genMaybe' = do
      a <- arbitrary
      -- frequency :: [(Int, Gen a)] -> Gen a
      frequency [ (1, return Nothing)
                , (3, return (Just a))]

If we don't use Hspec we can also indicate omit the type definitions for properties. Quickcheck will provide with:

    prop_additionGreater :: Int -> Bool
    prop_additionGreater x = x + 1 > x

    runQc :: IO () -- basically something like `main`
    runQc = quickCheck prop_additionGreater

Quickcheck also has some common edgecases (like `0`) which you should be aware of. `0` is always generated before the random sampling of `Num`s.

