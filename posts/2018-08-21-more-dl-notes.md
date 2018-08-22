---
layout: post
title: ML Notes\: autoencoders and dropout
---

Some quick reminders:
- **On Dropout:** Don't forget to remove dropout on validation! We don't want to overfit when training, but on inference we would like to use the full power of our model.
- **Autoencoders:** Currently, these _aren't_ good at lossless compression (obvious, since they output probabilistic data structures), and _aren't_ so good at video compression (which I specify because you could probably allow for _some_ loss-of-information).
  + That said, if I were to try solving that problem, I would mix autoencoders with neural architecture search -- with model size + compression size as the output (+ accuracy) as the bandit reward.
- **RNNs:** It seems like it's synonymous to say that a "generated sequence" is the same as "applying recursion." Intuitively, this seems incorrect since a sequence does not account for tree-structures or fancier things, like mutually recursive structures, but maybe I am missing something here.
  + When using least squares, normally we assume an iid process: $\sigmal_{t=0} (s_t = f(s_{t-1}))^2$, however when injecting temporal dependence with a recursive function, least squares becomes: $\sigmal_{t=1} (s_t = f(h_{t-1}, s_{t-1}))^2$ where $h_{t-1}$ is the hidden state of $f$, applied to the prior timestep.
  + This $h$, hidden state, is the "memory" and requires an initial object (or, co-terminal object). Using this terminology we can infer that the final, ideal parameters are the terminal object.
  + linear combinations are how we keep things in memory

Question: Is there a notion of things being "approximate" in CT? For instance, a neural network requires an initial parameter space and moves towards an ideal, terminal, parameter space. That said, it will never get there. It's the same with reinforcement learning and approximate optimization (which is probably the most formal of these three research areas) (edited)

Also:
  + [this paper on gradient clipping to avoid vanishing / exploding gradient](http://proceedings.mlr.press/v28/pascanu13.pdf)
  + [this blog post on the LSTMs](http://colah.github.io/posts/2015-08-Understanding-LSTMs/)
