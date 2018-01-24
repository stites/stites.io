---
layout: post
title: Clipping, that extra bump for loss criteria
---

For the past couple of days I've been writing a transfer-learning workflow for the dog breed identification kaggle competition. This was done solely in pytorch and torchvision, heavily referencing the beginner pytorch tutorials, and you can find them at [stites/circus](https://github.com/stites/circus/blob/master/notebooks/dog-breeds.ipynb). This code, which doesn't do anything fancier than a fastai notebook, was able to produce results that placed in the 50th-percentile of the competition. This isn't enough to qualify for anything like a bronze medal on kaggle (there are ~1k participants), but as a tool to familiarize myself with a CV pipeline from scratch, I would say this is a success.

There are four reasons why this workflow was successful and there are a plethora of improvements to be made. First, reasons how this was successful:

- Transfer learning from Resnet152 was used. Resnet34 alone got the model up to the 75th percentile, Resnet152 trained much faster and finished training (ie, the validation-training loss balance fell out-of-whack) at the 50th epoch. The final architecture consisted of Resnet152, followed by a fully-connected layer (fc layer) of 4096, relu, another fc layer of 120, and finally softmax. Sigmoid was attempted as well, however since the results are looking for a single breed, it is not as performant.

- Data Augmentation was used. This is a given and seems essential to any computer vision pipeline.

- A hacked-together version of Stochastic Gradient Descent with Restarts was used. This basically was just me reinitializing the learning rate scheduler at a regular interval.

- Clipping was used on the final result. Since there were 120 classes, the bounds were \\(0.01 / 120\\) and \\(0.99 / 120 \\). This dropped the softmax-based solution from the 60th to the 50th percentile.

That said, there are a number of places where this could be improved:

- Test-time augmentation could be used.

- I unfroze resnet152 so that it would have a better fit to dogs and not other imagenet images. While this is good theoretically, I think using differential learning rates would have allowed for a better speedup.

- On the note of other fastai improvements -- I'm less impressed with the learning rate finder, but it could have been a valuable asset to include.

- I should really work with an ensemble if I want to get better results. Doing a k-folds would be an excellent way to do this. Having each GPU work with its own model would also be adequate.

- I could do that thing where I save the fc-layer, train the full model on all of the validation set, then load the old fc-layer so that I am not overfitting on prediction, but am still able to use validation images to learn better features.

- Using a better architecture would be nice. In particular, I was interested in using Squeeze-and-Excite networks (this year's ImageNet winner).

- Using Cosine Annealing for the learning rate scheduler would have been nice and would move me from using a hack of SGDR, to real SGDR.

- A lot more data augmentation.

- Actually using Multi Class Log Loss instead of Cross Entropy Loss. This was done out of laziness.

- Using log softmax would make friendlier and faster gradients.

I think the most impressive thing was that, as alluded to in the title, clipping the loss function brought down the kaggle loss from ~1.0 to the current loss (0.5843).
