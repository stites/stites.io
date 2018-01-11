---
layout: post
title: (paper) Cyclical Learning Rates for Training Neural Networks
---

[Cyclical Learning Rates for Training Neural Networks][paper]

([github][gh])

Introduced via Jeremy Howard as one of the two techniques that gives him deep learning superpowers, cyclical learning rates are a departure from the manual decision making process when choosing a learning rate in deep learning. Currently either stepwise- or cosine-annealing is the usual method of learning rate decay, but these include decisions around manual hyperparameters which humans are not nessecarily good at making. Adaptive learning rates also exist, but cyclic learning rates require a
negligible computation overhead. Where adaptive methods like AdaGrad, RMSProp, and AdaDelta treat the learning rate problem as a non-stationary one, CLR offers a schedule which, counterintuitively, resets the learning rate periodically.

several policies for learning rate schedules are proposed in the paper. Here is the first in the paper, converted to haskell. Other policies might include the following triangular policy with a learning rate difference is cut in half each time, or where the decay rate decays by an exponential factor of \\(gamma^iteration\\).

    triangular_clr_policy :: forall f . (Ord f, RealFrac f) => f -> f -> Int -> Int -> f
    triangular_clr_policy maxLR minLR e s = lr
      where
        epoch_num, step_size, cycle, x :: f
        epoch_num = fromIntegral e
        step_size = fromIntegral s
        cycle = floor ( 1 + epoch_num / (2 * step_size) )
        x  = abs ( epoch_num / step_size - 2 * cycle + 1 )
        lr = lowerLR + (maxLR - lowerLR) * (max 0 (1 - x))

There are a few hyperparameter choices which can be now be automated: the epoch number and step size are usual suspects, but the min- and max-bound of learning rates can be approximated with one training run of the network for a few epochs. This is called the "LR range test" and involves letting your model run while letting the learning rate increase linearly between low and high LR values. This gives Accuracy-LR results which usually has a jagged tooth-like plot. Using bounds of the left and
right side of the tooth are your hyperparameters.

[adadelta-of-bandits]: https://arxiv.org/abs/1412.6599
[paper]: https://arxiv.org/abs/1506.01186
[gh]: https://github.com/bckenstler/CLR
