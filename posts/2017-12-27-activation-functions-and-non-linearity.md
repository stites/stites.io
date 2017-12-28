---
layout: post
title: Activation functions and non-linearity
---

if linear functions are \\(s = W x\\), then a 2-layer neural network function would look more like look more like:
$$ s = W_2 max(0, W_1 x) $$

Where \\(W_1\\) has the dimensions of input and an intermediate dimension and \\(W_2\\) mapping from the intermediate dimension to our output dimension. Notice that if we remove the element-wise \\(max\\) function then we would be able to collapse \\(W_2 W_1\\) into a single matrix.

Similarly, a three-layer neural network would look like:
$$ s = W_3 max(0, W_2 max(0, W_1 x)) $$

With the biological tilt, dendrites carry signal to a neuron's cell body where the signal accumulates. If the final signal sums above a certain threshold, the neuron 'fire, sending a spike along its axon. In the computational model, we assume that the precise timings of the spikes do not matter, and that only the frequency of the firing communicates information. Based on this rate code interpretation, we model the firing rate of the neuron with an activation function.

Activations functions are incredibly versitile. With the simple notion that a neuron can "like" (have an activation weight near 1.0) or "dislike" (a weight near 0.0) an input, we can encode a binary classifier (interpret activation as one class and `1-activation` as the contrasting class), a binary SVM classifier (attaching a max-margin hinge loss as the activation), or we can interpret the activation as kind of regularization (from a biological perspective: how easy it is to "gradually forget" a parameter).

Common activation function include the sigmoid function (which squashes numbers between [0,1]), tanh (which squashes numbers to the range of [-1,1]), relu (rectified linear units: which max(0,x) numbers), leaky relu, and maxout.

---

### Sigmoid and tanh

Sigmoid activation has fallen out of favor as an intermediate layer since they tend to saturate at either the tail of 1 or 0, which cause the gradient at those regions to zero-out. Gradients of neglegent magnitude fail to learning via backpropagation, blocking any updates from flowing through the network. On top of this, sigmoid outputs are not centered around zero. I think the reasoning behind how this is bad may be best explained in the Stanford CS231 course's page:

> This has implications on the dynamics during gradient descent, because if the data coming into a neuron is always positive (e.g. \\(x > 0\\) elementwife in \\(f = w^T x + b \\)), then the gradient on the weights \\(w\\) will during backpropagation become either all be positive, or all negative (depending on the gradient of the whole expression \\(f\\)). This could introduce undesirable zig-zagging dynamics in the gradient updates for the weights. However, notice that once these gradients are
> added up across a batch of data the final update for the weights can have variable signs, somewhat mitigating this issue. Therefore, this is an inconvenience but it has less severe consequences compared to the saturated activation probjem above.

The zig-zagging can be explained from the fact that a update may have to overcompensate for a negative value in a positive space when, later, a subsequent update will have to, again, overcomensate for a poor update. Tanh also suffers from problems of saturated gradients, but it is always preferred to sigmoid activation because of the latter issue.

### ReLU and it's variants (Leaky ReLU, PReLU, and Maxout)

ReLU (Rectified Linear Units) are probably the most popular at the time of this writing. These activations accelerate convergence of stochastic gradient descent by a factor of six (according to [Krizhevsky, et al.][kriz]) in comparison to sigmoid and tanh. This is supposidly due to the inexpensive bounds-check and unsaturating nature, as opposed to sigmoid and tanh which have expensive exponentials to compute and which have values saturate at the min- and max- bounds.

Unfortunately, given that ReLUs run a max(0, x) function and can possibly zero out a parameter during propagation, it is possible for a ReLU unit to irrevocibly die if a gradient is sufficiently large. In this case, the parameter will get knocked off of the data manifold. Countermeasures for this include ensuring that the learning rate is sufficiently low, and moving from ReLU to Leaky ReLU activation.

Leaky ReLU takes the form of \\(f(x) = 1(x < 0)(a x) + 1(x >= 0)(x)\\) with \\(a\\) being a small constant. Instead of zero-ing out \\(x\\) when \\(x < 0\\), the leaky ReLU function will instaed have a negative slope of \\(a\\) (usually something like 0.01). While this would solve the problem of dying activation parameters, the empirical results aren't always consistent. The slope can also be made a parameter of the neuron itself, which is seen in a PReLU neuron which is introduced in [Delving
Deep into Rectifiers, Kaiming He et al. (2015)][kai], but the consistency of results are still unclear.

Most promising is maxout ([Goodfellow, et al.][goodfellow]). A Maxout activation computes \\(max(w_1^T x + b_1, w_2^T x + b_2)\\) which is just the generalization of _both_ ReLU (when \\(w_1\\) and \\(b_1\\) are 0) and Leaky ReLU (Question! is this when the weights are 1 and \\(b_1\\) is \\(a\\)? I probably need to clarify the Leaky ReLU function). Maxout is nice in that it never suffers from saturation and operates on a linear range without zero-ing out any parameters -- so the dying parameter problem is avoided. That said, it comes at the cost of double the parameters and compute of ReLU, which is potentially even more costly than a sigmoid or tanh function.

---

When it comes to actually choosing your activation layers, the guidance from the Stanford CS213 class is this: use ReLU, if dying gradients become a problem, try tuning your learning rate or give Leaky ReLU and Maxout a try. Never use sigmoid, and you can experiment with tanh, but don't expect it to perform as well as Maxout or Leaky ReLU.


[kriz]: http://www.cs.toronto.edu/~fritz/absps/imagenet.pdf
[kai]: http://arxiv.org/abs/1502.01852
[goodfellow]: http://www-etud.iro.umontreal.ca/~goodfeli/maxout.html
