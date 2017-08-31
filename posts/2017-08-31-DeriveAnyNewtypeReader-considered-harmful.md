---
layout: post
title: DeriveAnyNewtypeReader considered harmful
---

So for the past couple of days I've been chasing down what I thought was a space leak introduced by monadic while loops. It's a reasonable thing to think because reinforcement learning in haskell requires nested monadic loops: one outer loop to iterate through all episodes, another nested loop to travel through all steps in each episode. A pattern which has been [documented][ro-che] [by][well-typed] [others][so] as potentially leaky but which, never-the-less, is still sometimes useful\*.

[ro-che]: https://ro-che.info/articles/2017-01-10-nested-loop-space-leak
[well-typed]: https://www.well-typed.com/blog/2016/09/sharing-conduit/
[so]: https://stackoverflow.com/questions/41306593/memory-leak-in-recursive-io-function-pap

It turns out that the culprit to this bug was _not_ a space leak but, rather, a problem with the conflict between `DeriveAnyClass` and `GeneralizeNewtypeDeriving`. GHC will throw out compiler warnings about this interaction, and it would be wise to dig deeper into what each one does. Here's some code which will exemplify what I've been dealing with:

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizeNewtypeDeriving #-}

data Config = Config { maxOinks :: Int }
  deriving (Eq, Show)

newtype Oink a = Oink { kniO :: Reader Config a }
  deriving (Functor, Applicative, Monad, MonadReader Config)
```

This code is perfectly fine, but if we make a slight modification, we introduce a bug:

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Generic #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizeNewtypeDeriving #-}

data Config = Config { maxOinks :: Int }
  deriving (Eq, Show, Generic ToJSON, FromJSON)

newtype Oink a = Oink { kniO :: Reader Config a }
  deriving (Functor, Applicative, Monad, MonadReader Config)
```

At this point, GHC 8.0.2 shouts at us, telling us that it can do what we want it to do but it'll use `DeriveAnyClass` before `GeneralizeNewtypeDeriving`, and while `GeneralizeNewtypeDeriving` is actually filling out code we would not write (lifting instance functions from our wrapped level to our newtype wrapper level), `DeriveAnyClass` is just adding an empty `instance ToJSON Config` line to our code. This works due to `default` functions in a typeclass. For instance, `ToJSON`'s instace looks like this:

```haskell
-- From aeson:

class ToJSON a where
    -- | Convert a Haskell value to a JSON-friendly intermediate type.
    toJSON     :: a -> Value

    default toJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions
```

So by declaring `instance ToJSON Config`, we use the defaulted `toJSON = genericToJSON defaultOptions` code if our code can satisfy `(Generic a, GToJSON Zero (Rep a))`.

The bug exists with MonadReader. Now that both pragmas exist in our module, `DeriveAnyClass` takes precedence and we get `instance MonadReader Config Oink`. Unfortunately, it turns out that, while there isn't default code, there is existing code which satisfies properties we've set out _by default_ so we wind up with a problem:

```haskell
-- From mtl-2.2.1

class Monad m => MonadReader r m | m -> r where
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL (ask | reader), local #-}
#endif
    -- | Retrieves the monad environment.
    ask   :: m r
    ask = reader id

    -- | Executes a computation in a modified environment.
    local :: (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

    -- | Retrieves a function of the current environment.
    reader :: (r -> a) -- ^ The selector function to apply to the environment.
           -> m a
    reader f = do
      r <- ask
      return (f r)
```

Look at that! we've just slipped through a `MINIMAL` constraint! This code will recurse infinitely and crash your machine in a fiery blaze.

So what is the solution? Well there are two: if you are on GHC-8.0.x, never put both of these pragmas in the same module (possibly more sternly, never use `DeriveAnyClass` in a module where there is a typeclass with a `MINIMAL`y annotated instance, but I need to doublecheck that). In GHC-8.2.x, you can now use dedriving strategies to determine how to get the instances you want. There are three types of strategies: `stock`, `newtype`, and `anyclass`. `newtype` and `anyclass` will work the ways detailed above while `stock` will generate code like `instance Eq` in the current way GHC generates code automatically to get your instance.

For more on the 8.2 improvements, I would redirect you to ryan scott's article: "[Improvements to deriving in 8.2][ryanglscott]."

[ryanglscott]: https://ryanglscott.github.io/2017/04/12/improvements-to-deriving-in-ghc-82/


\* - That being said, after a bit of reflection I'm strongly motivated to model these types of problems simply as functions working on `ListT`, if i am able to think about how to handle both finite and empty lists, as in the case with reinforcement learning.


