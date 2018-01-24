---
layout: post
title: 'Structured Deep Learning (alt: Embeddings are not just for words)'
---

Going through more of fastai -- Jeremy Howard points out that there is a huge gap between industry and academia where academia loves solving unsupervised learning problems and industry loves supervised ones (guess I'm lucky, working in semi-supervised learning). This is pretty easy to see since unsupervised learning is the task of understanding what you do not know, whereas supervised learning is just memorization. This point can be seen through [Andrew Ng and Geoffery Hinton's
interview on the deeplearning.ai channel][heros].

Structured deep learning seems to be a relatively industry-based endeavor, going largely unnoticed but with a simple premise: embedding all the things. Basically take each categorical feature, add extra dimensionality to it (Jeremy's recommendation is \\(min(50, (cardinality(feature) + 1) / 2)\\)) and combine everything as input to your network. Simple, generic, and flexible, while also able to acheive up a 3rd-place result in the Rossman Kaggle competition, right behind teams which spent 40 ([first place][first]) to 70 ([second place][second]) percent of their time on feature engineering, with the remaining time spent on feature selection (here's [even more detail][first-more] to the 1st place winner). The second place winner cites that it took more than 25 hours to build and predict the final model. By comparison the third place model [took 20 minutes to train one network on a gpu and the final model took 3.5 hours to ensemble 10 networks][third-kaggle].

The third place winner also wrote up his research into a paper, [Entity Embeddings of Categorical Variables][third-arxiv] (Cheng Guo, Felix Berkhahn), something to keep in mind for any future time series or ecommerce work.

[heros]:https://www.youtube.com/watch?v=-eyhCTvrEtE

[first]:http://blog.kaggle.com/2015/12/21/rossmann-store-sales-winners-interview-1st-place-gert/
[first-more]:https://www.kaggle.com/c/rossmann-store-sales/discussion/18024
[second]: http://blog.kaggle.com/2016/02/03/rossmann-store-sales-winners-interview-2nd-place-nima-shahbazi/
[third-arxiv]:https://arxiv.org/abs/1604.06737
[third-kaggle]:http://blog.kaggle.com/2016/01/22/rossmann-store-sales-winners-interview-3rd-place-cheng-gui/

[ruder-optimizations]:http://ruder.io/deep-learning-optimization-2017/
[ruder-embeddings]:http://ruder.io/word-embeddings-2017/


