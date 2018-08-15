---
layout: post
title: Different ways to slice a network when transfer learning
---

Some guidelines for how to slice a network when transfer learning.

- If the dataset is small and the new training data is similar to the original network's data: Slice off only the last layer (not the entire head).
- If the dataset is small and the new training data is different to the original network's data: Slice off the entire head and replace it with ONE fully-connected layer.
- If the dataset is large (up to your discretion) and the new training data is similar to the original data: Slice off the entire head and, matching the architecture, replace it with a new fully-connected network.
  + For the convolutional layers: experiment with freezing the backbone or using differential learning rates.
- If the dataset is large and the new training data is different to the original data... Well this is going to be a wild-card. Randomly initialize the head, and for the convolutional layers it is up to you whether you want to redo everything (initializing from scratch) or if you want to use the current weights as initial parameters.

More: [How transferable are features in deep neural networks][paper] -- NIPS 2014 -- Yosinski, Clune, Bengio, Lipson

[paper]:https://arxiv.org/pdf/1411.1792.pdf
