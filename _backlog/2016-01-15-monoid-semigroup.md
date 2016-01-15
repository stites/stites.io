
so aside from monoid

Why isn't Integer a monoid? mappend isn't specific enough.
  + Integers form a monoid under summation and multiplication (monoid is defined as a
    _set with some binary operation_.
  + We can actually define a newtype Sum or Product which will make Integer a monoid!
  + Lists form a monoid under concatenation, (but also something else?)

newtype is hard to justify or explain to people that don't understand how Haskell
compiles code!!! --> My interpretation is that newtype is compiled into a symbol,
reified at pre-compile-time.

--> turns out it is distinct at compiletime and not runtime.
--> you would use newtype over data because data is _lifted_ and, thus, also contain
bottom. Thus:
    data `Any Bool` contains: Any True , Any False , Any (Bottom), (Bottom)
    newtype `Any Bool` contains: Any True , Any False , Any (Bottom)

The evaluation of a bottom happens due to laziness. This might mean you think you
can strict-y-fy everything, however this is not the case for... check out the wiki
for more.

note: lifting === boxing

newtype is a "single-member C union" that avoids creating an extra pointer, but still
gives you a new type and data construcor

use newtype for signalling intent - newtypes cannot turn into sum- or product- types
, improve typesafety - you can't mix them up with their isomorphic relation, add
different typeclasses to things that are (normally unchangable?)

From Data.Monoid, you get the following:

    Sum, Product, All, Any

=================================

A monoid variant that provides even stronger guarentees is the abelian, or communtative, monoid.

folding => catamorphism
typeclass => interface(ish)

    foldr mappend mempty ([2,3,4,5] :: [Product Int])

Data.Monoid also includes `Last` and `First` for Maybes:

    $ Last Nothing <> Last (Just 2)
    Last {getLast = Just 2}
    $ First Nothing <> First (Just 2)
    First {getFirst = Just 2}
    $ First Nothing <> First Nothing
    First {getFirst = Nothing} -- same with Last


===================

law number two: for your binary operation, aka Monoid, it must be associative _and_
have a sensible identity value.
