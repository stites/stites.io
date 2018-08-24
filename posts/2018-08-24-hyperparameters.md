---
layout: post
title: Thoughts about Hyperparameters in DL
---

There are really two classes of these: optimizer hyperparameters (learning rate, number epochs, minibatch size, optimizer), and model hyperparameters (number and size of layers). From what I've seen optimizer hyperparameters are the parameters where there will one day be algorithm solutions to removing them from your model. Things like learning rate finders and cyclical learning rates ([Cyclical Learning Rates for Training Neural Networks][lrf], Smith), adaptive batch sizes ([AdaBatch][abs], Devarakonda, et al), and convergence properties (really just borrowing from reinforcement learning literature, but [Super-Convergence][cp] - Smith, Topin seems to include a proof for convergence) are lines of research with varying sucess at removing these. On the other hand, neural architecture search (NAS) -based techniques are coming more popular with packages like AutoKeras to eliminate model hyperparameters.

[lrf]: https://arxiv.org/abs/1506.01186
[abs]: https://arxiv.org/abs/1712.02029
[cp]: https://arxiv.org/abs/1708.07120
