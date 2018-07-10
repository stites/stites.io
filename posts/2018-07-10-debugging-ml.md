---
layout: post
title: Machine Learning Debugging, Unit Tests, and Production Torch
---

There are various tips when debugging machine learning algorithms, which recently [came up in the Torch gitter channel][gitter]. Documenting some of these words of wisdom:
+ Write unit test with fixed seeds and prefilled tensors for a step or two
+ You may also get to rollout half the epoch or episode (depending on your domain, episodes only if you can adjust the environment) and then unit test some more.
+ In general stepping through your code with a debugger comes first, unit tests are just to codify that.
+ From [Matthew Rahtz's blog][http://amid.fish/reproducing-deep-rl]: keep a daily journal of your assumptions and actions. This helps with logic bugs and will keep you sane. Also "log everything" and "chart everything" depending on how crazy things get.
+ If a model doesn't learn, it will predict same thing most of the time.
+ Debugging a model is a tedious process since it can be a small bug or whole model architecture incorrectly chosen and applied to your problem space. Start with data preprocessing and make sure you do it right, check inputs and whether they align with your target values. Then double check your loss function, is it the right one for your task? Most of the time it is likely a preprocessing bug or wrong loss function application.
+ On the note of data preprocessing, there are [these tips from CS231n][cs231n]. Their recommended preprocessing step is to center the data to a mean of zero, and normalize its scale to [-1, 1] along each feature
+ Also, to keep in mind that preprocessing must be computed on the training data and then applied to the training, test, and validation sets. This is definitely more of an "ML in the wild" tip and is ignored for numbers games like academic benchmarks.

[Koen Dejonghe](https://github.com/koen-dejonghe) pointed out these [tips from BYU's Perception, Control and Cognition Laboratory][byu] ([perma][byu-perma]). Looking at them specifically for debugging and testing:

+ Whiten (normalize) your input data.
For training, subtract the mean of the data set, then divide by its
standard deviation. The less your weights have to be stretched and
pulled in every which direction, the faster and more easily your 
network will learn. Keeping the input data mean-centered with constant
variance will help with this. You’ll have to perform the same
normalization to each test input as well, so make sure your training
set resembles real data.

+ Scale input data in a way that reasonably preserves its dynamic range.
This is related to normalization but should happen before normalizing.
For example, data x with an actual real-world range of [0, 140000000]
can often be tamed with tanh(x) or tanh(x/C) where C is some constant
that stretches the curve to fit more of the input range within the
dynamic, sloping part of the tanh function. Especially in cases where
your input data may be unbounded on one or both ends, the neural net
will learn much better between (0,1).

+ Overfit! The first thing to do if your network isn’t learning is to overfit a training point. Accuracy should be essentially 100% or 99.99%, or an error as close to 0. If your neural network can’t overfit a single data point, something is seriously wrong with the architecture, but it may be subtle. If you can overfit one data point but training on a larger set still does not converge, try the following suggestions.

+ Lower your learning rate. Your network will learn slower, but it may find its way into a minimum that it couldn’t get into before because its step size was too big. (Intuitively, think of stepping over a ditch on the side of the road, when you actually want to get into the lowest part of the ditch, where your error is the lowest.)

+ Raise your learning rate. This will speed up training which helps tighten the feedback loop, meaning you’ll have an inkling sooner whether your network is working. While the network should converge sooner, its results probably won’t be great, and the “convergence” might actually jump around a lot. (With ADAM, we found ~0.001 to be pretty good in many experiences.)

+ Decrease (mini-)batch size. Reducing a batch size to 1 can give you more granular feedback related to the weight updates, which you should report with TensorBoard (or some other debugging/visualization tool).

+ Remove batch normalization. Along with decreasing batch size to 1, doing this can expose diminishing or exploding gradients. For weeks we had a network that wasn’t converging, and only when we removed batch normalization did we realize that the outputs were all NaN by the second iteration. Batch norm was putting a band-aid on something that needed a tourniquet. It has its place, but only after you know your network is bug-free.

+ Increase (mini-)batch size. A larger batch size—heck, the whole training set if you could—reduces variance in gradient updates, making each iteration more accurate. In other words, weight updates will be in the right direction. But! There’s an effective upper bound on its usefulness, as well as physical memory limits. Typically, we find this less useful than the previous two suggestions to reduce batch size to 1 and remove batch norm.

+ Check your reshaping. Drastic reshaping (like changing an image’s X,Y dimensions) can destroy spatial locality, making it harder for a network to learn since it must also learn the reshape. (Natural features become fragmented. The fact that natural features appear spatially local is why conv nets are so effective!) Be especially careful if reshaping with multiple images/channels; use numpy.stack() for proper alignment.

+ Scrutinize your loss function. If using a complex function, try simplifying it to something like L1 or L2. We’ve found L1 to be less sensitive to outliers, making less drastic adjustments when hitting a noisy batch or training point.

+ Scrutinize your visualizations, if applicable. Is your viz library (matplotlib, OpenCV, etc.) adjusting the scale of the values, or clipping them? Consider using a perceptually-uniform color scheme as well.

---

Closely following this discussion was a discussion of "the PyTorch-to-Tensorflow production workflow" which, thankfully, I have never needed to deal with, but I thought it would be important to touch on since it has its own idiosyncrasies. In short, a common workflow (to the channel) is that a researcher will use PyTorch for prototyping, then migrate the model into TensorFlow for production use, but tensorflow models exhibit different behaviour with the same hypeparameters so you basically need to redo your hyperparameters tuning all over again. This is reminicent of [Deep Reinforcement Learning that Matters][drltm]. In this particular case, TF models refused to train unless you l2-normalized the input (unlike in Torch). Another researcher who doesn't need to train tensorflow models on the field simply trains models in PyTorch, then copies the weights to a mirror tensorflow model. A few things need to be rewritten in TF to make them Torch friendly (Bi-directional LSTM had a number of subtle differences), but said development in PyTorch was worth the hassle. This still might have nuances (like the data preprocessing requirements) and should always be backed with benchmarks.

[drltm]: https://arxiv.org/abs/1709.06560
[cs231n]: https://cs231n.github.io/neural-networks-2/#datapre
[gitter]: https://gitter.im/torch/torch7?at=5b3c4a6fbd92d8078291b5b7
[byu]: https://pcc.cs.byu.edu/2017/10/02/practical-advice-for-building-deep-neural-networks/amp/
[byu-perma]: https://perma.cc/5PNL-UZ9K
