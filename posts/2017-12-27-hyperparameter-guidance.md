---
layout: post
title: CITATIONS MISSING Hyperparameter guidance for ANNs
---

When writing your next neural network you'll need to go about selecting certain hyperparameters like number of layers, the size of each layer, how many convolutional layers, and how large your filter sizes should be.

Theoretically, a single neural network with a single hidden layer is a universal approximator to any function ([theory1][theory1], [alternate explanation][theory2]). Intuitively, as well, we would assume that the less complicated the model, the less likely we will overfit our training data. However, keep in mind that when we increase the size and number of layers, the capacity to fit larger and more complex functions increases.

It turns out that simpler models are not flexible enough to use in practice -- in part because neural networks are not convex functions. While smaller neural networks contain fewer local minima / maxima, it's easier to fit to one of them. In contrast, in larger neural networks you have thousands to hundreds of thousands of parameters and, while there are more local maxima possible, it is less likely to settle on a local maxima which will satisfy all parameters.

To combat overfitting methods like regularization ([usually L2 regularization][quora]), dropout, and input noise are used. So the rule of thumb here is that you should preference as many layers and neurons as compute time will allow.

Empirically, from the Stanford cs231n notes, a fully-connected 3-layer neural networks outperforms 2-layer networks and anything deeper than that is usually is not an effective use of computation. This is the opposite for convolutional networks, where depth is found to be a very important part of representational learning. This is because, in things like computer vision and, more recently, natural language processing, hierarchical representations. For instance, a face is made of eyes, a nose, and a mouth, which are all composed of edges, angles, and curves. In NLP, a document of text may be composed of paragraphs, which are composed of sentences, which are composed of shingles of characters or words.

Convolutional filters are another point of interest. Empirically, it turns out that you should always use 3x3 filters according to [\_\_\_\_\_\_][filters]. 

For more on this subject, Stanford's cs231 class points us in the direction of:

  - [Deep Learning book in press by Bengio, Goodfellow, Courville, in particular Chapter 6.4.][0]
  - [Do Deep Nets Really Need to be Deep?][1]
  - [FitNets: Hints for Thin Deep Nets][2]



[cs231]:https://cs231n.github.io/neural-networks-1/
[theory1]:http://www.dartmouth.edu/~gvc/Cybenko_MCSS.pdf
[theory2]:http://neuralnetworksanddeeplearning.com/chap4.html
[fastai]: lecture-3
[quora]: quora
[filters]: filters-paper
[0]:http://www.deeplearningbook.org/
[1]:http://arxiv.org/abs/1312.6184
[2]:http://arxiv.org/abs/1412.6550
