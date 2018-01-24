---
layout: post
title: Sigmoid activation
---

Use sigmoid activation for multilabel classification (not softmax!) in your output layer for multilabel classification and use binary crossentropy for your loss/cost function. Sigmoid, unlike softmax, doesn't give you a probability distribution around the number of classes, and so doesn't assume a univariate distribution. This is, essentially, using the same mechanics as logistic regression but doen't normalize across the total output classes. Use a cutoff of 0.5 to determine if something is
in- or out-of class.
