---
layout: post
title: Thoughts about Hyperparameters in DL
---

There are really two classes of these: optimizer hyperparameters (learning rate, number epochs, minibatch size, optimizer), and model hyperparameters (number and size of layers).

From what I've seen optimizer hyperparameters are the parameters where there will one day be algorithm solutions to removing them from your model. Things like learning rate finders and cyclical learning rates ([Cyclical Learning Rates for Training Neural Networks][lrf], Smith), adaptive batch sizes ([AdaBatch][abs], Devarakonda, et al), and convergence properties (really just borrowing from reinforcement learning literature, but [Super-Convergence][cp] - Smith, Topin seems to include a proof for convergence) are lines of research with varying sucess at removing these. Optimizer hyperparameters can also be ingrated directly into the optimizer itself, as can be seen with Adam and Adagrad for learning rates. On the other hand, neural architecture search (NAS) -based techniques are coming more popular with packages like AutoKeras to eliminate model hyperparameters.

---

Other notes:
- Small minibatch sizes have more noise in error calculations, so you should preference for higher minibatches. Bumping batch size in powers of 2 usually plays well with the hardware architecture you are using as well. Generally start with something in the 64-2048 range depending on your type of data.
- Early stopping (ie - convergence) is usually determined by ensuring that validation error is always decreasing my some statistic for a window of size. Since you would be working with validation error, this will usually be more noisy than training error -- which is why a window is used.
- Number of hidden units. This seems like a black art. You need to have enough neurons to capture the complexity of the problem, but not so much that the model quickly overfits on your training data. This can be seen when the difference between your model's training error is much lower than its validation error. If this happens, throw more dropout at the model.
- As Karpathy suggests, in general three layers is enough for fully-connected layers (3 is better than 2, usually, and greater than 3 has decaying returns). With convolutional layers, you can never seem to have enough depth.
- for RNN unit types: GRUs and LSTMs seem to be neck-and-neck on character modelling and, in other contexts, it's really a decision that is left to the user (although results skew to LSTMs) -- but both are better than vanilla RNNs.
- stacking RNN units is also highly contextual. Character modelling seems to lose power after 2 stacked units, while acoustic-to-word LSTM models are SOTA at a depth of 5 or 7 models.
- Word embeddings, empirically, [tend to be most effective with a dimensionality between 50 and 200][word-embeddings]. Although it is not unusual to seem embeddings with a dimenstionality of 500 or 1000.

[Also](https://arxiv.org/abs/1206.5533),
[also](http://www.deeplearningbook.org/contents/guidelines.html),
[also](http://neuralnetworksanddeeplearning.com/chap3.html#how_to_choose_a_neural_network's_hyper-parameters), 
[also](http://yann.lecun.com/exdb/publis/pdf/lecun-98b.pdf), 
[also](https://arxiv.org/abs/1606.02228),
[also](https://arxiv.org/abs/1506.02078).


[word-embeddings]:https://arxiv.org/abs/1507.05523
[lrf]: https://arxiv.org/abs/1506.01186
[abs]: https://arxiv.org/abs/1712.02029
[cp]: https://arxiv.org/abs/1708.07120
