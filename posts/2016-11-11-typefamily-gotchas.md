---
layout: post
title: Typefamily gotchas
---

{{ page.title }}
================

Typeclasses are an amazing abstraction in haskell. They are one of the
fundamental building blocks in sharing and managing code. Among other points of
mention, some of the benefits typeclasses have over OO-styled interfaces include
the ability to create objects out of nothing (like `mempty` for the `Monoid`
typeclass) and the ability to lazily add polymorphism at compile time (ie:
`fromInteger :: Integer -> a` in which we define some `fromInteger 1 :: Double`
later on in the program.

Typefamilies allow us to introduce associated types to typeclasses, bringing our
code to another level by having locally defined type-dependence.

```
class MakesSeries s where
  type Emitted s

data State = A | B

instance MakesSeries State where
  type Emitted State = String
```

Notice that the typefamily defines a type-level function: when you apply
a `State` to an `Emitted`, you get out `Text`. Here, we're building out
a typeclass that can take in some defined `State` (generically, just `s`), and
each time you step through the series, it emits something. It could be text
(like right now), or json, ints... anything you'd like.

If we're not dealing with text, however, it might be worth having some way to
define how to serialize things that are `Emitted`:

```
class MakesSeries s where
  type Emitted s
  serialize :: s -> Emitted s -> String

data State = A | B

instance MakesSeries State where
  type Emitted State = Int

  serialize _ int = show int
```

But we don't actually care about what the state is, we just care about how we
`show` the emitted values. Can't we just drop it?

```
class MakesSeries s where
  type Emitted s
  serialize :: Emitted s -> String
```

Error, nope! Why is this? Well, `serialize` doesn't _uniquely_ define an `s`
anymore! What if we had some `instance MakesSeries State2`? The compiler
wouldn't understand which `Emitted s` it should be using because (I'm guessing)
there is eta reduction happening behind the scenes! We can fix this two
different ways.

In the first way, we move our type family to a data family. Basically, this is
the same as a type family, except we are creating an ADT instead of a type alias
with our function:

```
class MakesSeries s where
  data Emitted s                        -- notice the change here
  serialize :: Emitted s -> String

data State = A | B

instance MakesSeries State where
  newtype Emitted State = Value Int

  serialize (Value int) = show int
```

But if we have to write lots of emitted values, that's going to be a handful of
repeated data constructions.

Alternatively, we can use a `Proxy` from `Data.Proxy`:

```
class MakesSeries s where
  type Emitted s
  serialize :: Proxy s -> Emitted s -> String

data State = A | B

instance MakesSeries State where
  newtype Emitted State = Value Int

  serialize _ int = show int

-- ...later, in action:
foobar = serialize (Proxy :: Proxy State) 2
```

For my use-case, I opted for the latter. You can see the code at
[stites/test-machines](https://www.github.com/stites/test-machines). Another
thing you might see is the use of a proxy for a `ScopedTypeVariables`
declaration. This was used so that I can cast some kind of `mempty`-defined
value without having to pass in an actual parameter.

On a tangential noted, there's a lot of type-level hackery you can get away with
in Haskell, and I should write something on `TypeRef` later for dynamic,
dispatch-styled typing.





