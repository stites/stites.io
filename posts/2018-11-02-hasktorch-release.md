---
layout: post
title: Hasktorch v0.0.1
---

# DRAFT

*Long story short:* After a few prototypes and feedback, I'm happy to announce that `hasktorch-0.0.1.0` is now officially on Haskage! The API documentation [can be found here][indef]. The rest of this post introduces the library, goes over some caveats, shows you how to build a fully-connected XOR predictor, talks a bit about internals, and goes over caveats as well as what future development entails.

---

The Hasktorch suite of libraries includes the haskell bindings to PyTorch's backend, in the `hasktorch-ffi-*` packages, as well as the beginnings of generic backpack modules, [see `hasktorch-indef`][indef], which constitute a simple Haskell interface to a modern deep learning library which constructs dynamic computation graphs. This interface comes in two flavors: a dynamic interface, much like what you would expect out of something like PyTorch, as well as a statically-typed interface, which begins to make use of Haskell's dependent types to statically enforce the safe construction of these dynamic graphs.

[indef]: https://hackage.haskell.org/package/hasktorch-indef

Hasktorch differs from the Tensorflow bindings from the fundamental level of how DAG construction is formed for computation. This means that tensors are untethered from a computational context and do not require a `Session`-like monad in order to run. Austin and I have also prioritized the static tensor interface as well so we have fully-typed dimensions, making use of Artem's [`dimensions`][dimensions] library.

Within the Haskell community, there is a growing collection of pure-haskell alternatives such as [linear][linear], [grenade][grenade], and [backprop-learn][backprop-learn], [easytensor][easytensor]. Hasktorch takes inspiration from these and, hopefully, will offer a comparable API that it will also be able to keep up with the fast pace of the deep learning community.

[easytensor]: https://hackage.haskell.org/package/easytensor
[backprop-learn]: https://github.com/mstksg/backprop-learn/
[linear]: https://github.com/ekmett/linear
[grenade]: https://github.com/HuwCampbell/grenade
[artem]: https://github.com/achirkin
[dimensions]: https://hackage.haskell.org/package/dimensions

## Getting started

To begin you need a copy of the "A Tensor" library (ATen), PyTorch's backend. Hasktorch is currently pinned to the last commit of ATen which has externed broadcasting functions, which you can find [at our fork](https://github.com/hasktorch/ATen). We also have build scripts which you can find in [`ffi/deps/build-aten.sh`](https://github.com/hasktorch/ffi/blob/be11dc3f737dd1b4690234449e6071dfb05ce9d9/deps/build-aten.sh). You'll need cabal's `v2-build` for [backpack][backpack] support (`stack` will not work here, as far as I am aware), ghc >= 8.4, and a ~~stiff drink~~ flare for hacking.

Because of the dependency on a dynamic-linked shared object file and because we have veered away from the stack lts system, you can find the `cabal.project.freeze` files and `cabal.project.local` files that the developers use in [our cabal folder][cabal-folder].

A short script to get started could look like this:

```
# git clone https://github.com/hasktorch/hasktorch.git --recursive
# cd hasktorch
# (cd ffi/deps && ./build-aten.sh)
# ln -fs cabal/project.freeze-8.6.1 cabal.project.freeze
# ln -s cabal/project.local-cpu     cabal.project.local
# echo "and read the annotated local file"
# most cabal.project.local
# cabal v2-update
# cabal v2-build all
```

[cabal-folder]: https://github.com/hasktorch/hasktorch/tree/master/cabal
[backpack]: https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst

## Let's predict XOR!

Taking an example out of [Deep Learning][deep-learning] (Goodfellow, Bengio, Courville), learning XOR is a task which cannot be solved linearly, but is made tractable with a simple two-layer nerual network with rectified linear unit (ReLU) activation.

[deep-learning]:http://www.deeplearningbook.org/
<!--
@book{Goodfellow-et-al-2016,
    title={Deep Learning},
    author={Ian Goodfellow and Yoshua Bengio and Aaron Courville},
    publisher={MIT Press},
    note={\url{http://www.deeplearningbook.org}},
    year={2016}
}
-->

In Hasktorch there are various mechanisms to build tensors. Most of these return values lists are in IO For instance we can make a...


**TODO: GO OVER REST OF** https://github.com/hasktorch/hasktorch/blob/master/examples/xor/Main.hs

- walk through tensor creation of data
- PSA that tensor creation is the last pure function still in IO
  + FFI is always in IO, varying degrees of optimization
  + (\*) We may want to multithread tensor creation for large tensors - looking at ST or more type-correct abstractions.

- Show the random tensor functions
  + Explain the heuristics for "C-function" (prefixed `_`), "in-place function" (postfixed `_`), and "pure function" (no )

- Intro to `backprop` library
  + (\*) Discuss that there is "manual calls to torch backprop." -- talk about at potential future develoment which adheres to diffhask, potentially vector-space(?).

- Remainder of the example.

## Internals: Backprop
 
Maybe good to explain backprop in more depth. Redirecting to Justin's work, talking about what Conal is up to, etc.

## Internals: Backpack
 
Probably important to give a brief introduction to backpack and explain what an "indefinite" is. **Important to discuss how backpack allows us to iterate on a better user interface and how it also gives us a target for pure-haskell backends**.

## Internals: Singletons and Dimensions
 
Simple discussion of the sorts of things we can and cant do. Maybe touch on Naperian functors for broadcasting.

## Current Status and Caveats

As you can probably tell from the caveats, while hasktorch is feature complete there is still a lot of work to be done. Currently you can build simple neural networks, and some Computer Vision combinators exist (conv1d, conv2d, max-pooling), but most are unfinished since it is a manual effort of encoding typechecks into the user interface. We are moving into a python-driven phase of development where we can build python models in pytorch, and have their comparable hasktorch model built alongside them. Working on this will make you intimately familiary with the ATen codebase, potentially with cuda internals.

Caveats:

- memory usage _can be_ comparable to PyTorch from what I can tell. It's hard to know for sure since we don't have good dataloaders: the final  "production" model of LeNet loads all of CIFAR10 into the codebase. This, as well as datavisualization via tensorboard, needs a bit of work.

- We currently lack a safe in-place abstraction. Because mutation is vital for large tensor opreations, Hasktorch devs are of the opinion that it is better to move fast and let users get access to these sorts of operations, but we need to iterate on these sooner rather than later. It should be straight forward, using ST, with some future use-cases for linear types (when type-level dimensions change, but the foreign pointer remains the same).


## Call for Collaboration

These things will all be discussed in more detail above. Immediate, simple help:

- Make more of the statically typed interface
- Codifying the mutable aspects of tensors (look to MVector for API)
- Bring back support for GPUs (it's all there, but we need to marshall custom lists in `tensordata` and `newWithStorage` functions).
- Make more user-friendly NN functions via python development.
- custom expanding with Naperian functors (: Cudos to Jeremy Gibbon for this

Long-term, call-for-collaboration:
- Prototype C++ bindings with `inline-c-cpp`
- Work on top of Crayon-hs https://github.com/austinvhuang/crayon-hs
- Looking to linear types to fix some of the 









```
-- make an exact datapoint of all 4 examples of XOR
mkExactData :: IO (Tensor '[4, 2], Tensor '[4, 1])
mkExactData = 
  (,)
  <$> unsafeMatrix
    [ [0, 0]
    , [0, 1]
    , [1, 0]
    , [1, 1]
    ]
  <*>  unsafeMatrix
    [ [0]
    , [1]
    , [1]
    , [0]
    ]
```


```
-- xor/Main.hs
main :: IO ()
main = do
  section "Deterministic Inference" deterministic
  section "Stochastic Training"     stochastic
  section "Stochastic Training with Backprop" stochasticBackprop

  where
    section title action = do
      putStrLn ""
      putStrLn $ replicate 20 '='
      putStrLn title
      putStrLn $ replicate 20 '='
      putStrLn ""
      action

    deterministic = do
      (xs, ys) <- mkExactData
      net0 <- mkExact
      putStrLn "Inferring with the exact XOR function. True values:"
      print ys
      (ys', _) <- xorForwardIO net0 xs
      putStrLn "Inferred values:"
      print ys'
      printf "All values are identical? %s\n" (show . allOf $ eqTensor ys ys')
      (l, _) <- mSECriterionIO ys ys'
      printf "Mean-squared error: %s\n" (show (get1d l 0))

    seed0 = MWC.toSeed . V.fromList $ [0..256]

    stochastic = do
      net0 <- mkUniform
      let Just lr = positive 0.2
      (_, _, net) <-
        trainerIO lr 10000 (0, seed0, net0)

      (xs, ys) <- mkExactData
      (ys', _) <- xorForwardIO net xs
      putStrLn "\nInferred values:"
      print ys'

      (l, _) <- mSECriterionIO ys ys'
      printf "Mean-squared error: %s\n" (show (get1d l 0))
      where
        trainerIO lr n (c, s, net)
          | c >= n = pure (c, s, net)
          | otherwise = do
            (s', xs, ys) <- xorBatch s
            (o, grad) <- backwardIO net ys xs
            trainerIO lr n (c+1, s', update net (lr, grad))

    stochasticBackprop = do
      net0 <- mkUniform
      let Just lr = positive 0.01
      (_, _, net) <-
        trainer lr 2500 (0, seed0, net0)

      (xs, ys) <- mkExactData
      let (ys', _) = backprop2 xorForward net xs
      putStrLn "\nInferred values:"
      print ys'

      let (l, _) = backprop (Bp.mSECriterion ys) ys'
      printf "Mean-squared error: %s\n" (show (get1d l 0))
      where
        trainer lr n (c, s, net)
          | c >= n = pure (c, s, net)
          | otherwise = do
            (s', xs, ys) <- xorBatch s
            let (loss, (grad, _)) = backprop2 (Bp.mSECriterion ys .: xorForward) net xs
            printf "\rloss: %f" (fromJust $ get1d loss 0)
            trainer lr n (c+1, s', update net (lr, grad))

-- Forward + AD in backprop
xorForward
  :: Reifies s W
  => BVar s XORArch
  -> BVar s (Tensor '[4, 2])  -- ^ input
  -> BVar s (Tensor '[4, 1])
xorForward arch inp
  = Bp.linearBatch (arch ^^. _1) inp
  & Bp.relu
  & Bp.linearBatch (arch ^^. _2)

-- Simple way to update a network with a multiple of the gradient
update :: XORArch -> (Positive HsReal, XORArch) -> XORArch
update (l1, l2) (plr, (g1, g2)) = (l1 - (g1 ^* lr), l2 - (g2 ^* lr))
  where
    lr = positiveValue plr

-- ========================================================================= --
-- Initialize an architecture
-- ========================================================================= --

-- make the exact solution to this architecture
mkExact :: IO XORArch
mkExact = do
  b1 <- unsafeVector [0, -1]
  w2 <- unsafeMatrix [[1], [-2]]
  pure (Linear (constant 1, b1), Linear (w2, constant 0))

-- make the stochastic initial weights for this architecture
--
-- FIXME: may require tuning
mkUniform :: IO XORArch
mkUniform = do
  g <- newRNG
  manualSeed g 1
  let Just rg = ord2Tuple (0, 1)
  l1 <- fmap Linear $ (,) <$> uniform g rg <*> uniform g rg
  l2 <- fmap Linear $ (,) <$> uniform g rg <*> uniform g rg
  pure (l1, l2)

-- make a trainable batch of XOR samples (with uniform distribution of the 4 examples)
xorBatch
  :: MWC.Seed
  -> IO (MWC.Seed, Tensor '[4, 2], Tensor '[4, 1])
xorBatch s =
  (s',,) <$> unsafeMatrix xs <*> unsafeMatrix ys
 where
  (s', xs, ys) = runST $ do
    g <- MWC.restore s
    (xs, ys) <- xorLists g 4
    (, xs, ys) <$> MWC.save g

  -- Make n-number of XOR examples in ST
  xorLists :: GenST s -> Int -> ST s ([[HsReal]], [[HsReal]])
  xorLists g n =
    foldl
      (\memo ((x0, x1), y) -> ([x0,x1]:fst memo, [y]:snd memo))
      ([], [])
    <$>
      replicateM n (mkRandomXOR g)

  -- make a uniform random datapoint of XOR
  mkRandomXOR :: GenST s -> ST s ((HsReal, HsReal), HsReal)
  mkRandomXOR g = do
    x0 <- MWC.uniformR (0, 1) g
    x1 <- MWC.uniformR (0, 1) g
    pure ((fromIntegral x0, fromIntegral x1), x0 `xor` x1)
   where
    xor :: Int -> Int -> HsReal
    xor a b
      = fromIntegral
      . fromEnum
      . odd
      $ a + b
```


