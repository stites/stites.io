---
date: 1900-01-01
---


Conditional Harmoic Mixing (CHM):

works on a dag, a conditional probability matirx is associated with each link,
number of classes can vary from node-to=node. The posterior class probability at
each node is updated by minimizing the Kullback-Leibler divergence between the
node's distribution and its neighbors.

> KL divergence is the measure of the difference between two probability
> distributions. similar to the Kolmogorov-Smirnov test.  P is the ideal, Q is
> the approximated. Translated into the KL divergence, P is also the posterier
> and Q is the prior.  the KL divergence can be intuited as the amount of
> information lost when Q is used to approximate P.

Other graphical models include: probabilistic graphical models (bayes nets), and
semi-supervised learning on graphs (Laplacian SSL).

> Bayes nets: you know about this one.

> Laplacian Semi-supervised learning (SSL): an approach to ssl based on gaussian
> random fields model. labelled and unlaveled data are represented as vertices
> in a weighted graph, edges encode similarities between instances. The problem
> is then encoded as a gaussian random field on the graph (mean of the faield is
> characterized in terms of harmonic functions and then we use matrix operations
> to yeild results).

- Paraphased from [Semi-Supervised Learning Using Gaussian Fields and Harmonic
Functions][laplacian].

Laplacian SSL is more closely related to random walks on networks. In this
example, probabalistic models are indirect. In Bayes nets, probablistic models
are central. in Conditional Harmonic Mixing, we have an intermediate between the
two.

[laplacian]: http://mlg.eng.cam.ac.uk/zoubin/papers/zgl.pdf

