---
layout: post
title: Deep Reinforcement Learning that Matters (1709.06560)
---

A quick write up of some notes on [Deep Reinforcement Learning that Matters][drltm] that I took on the plane.

So the paper itself focuses on Model-Free Policy Gradient methods in continuous environments and is an investigation into how reproducing papers in the Deep Reinforcement Learning space is notoriously difficult. The authors discuss various failure cases that any researcher will be privy to when trying to implement work, and the shortcomings of the majority of authors who follow standard publication practices. Primarily, the key factors in DRL that hinder
reproducability stem from non-determinism and the large data requirements surrounding this class of algorithm.

The authors also reference questions of reproducability and good experimental practice in related fields ([Wagstaff 2012][wagstaff] -- [pdf][wagstaff-pdf], [Stodden, et al. 2014][stodden] -- [pdf][stodden-pdf], [perma][stodden-pdf-perma]), and go on to introduce better evaluations methods in benchmarks -- evaluating TRPO (with conjugate gradient descent w/ the KL constraint) and PPO due to their use of constraints and advantage estimation, as well as DDPG and ACKTR (a Kronecker-factored trust region) for their use of actor-critic methods with Monte-Carlo rollouts.

Henderson, et al. set out to investigate issues in the following:
  - the effect of specific hyperparameters, if not tuned properly.
  - differences due to codebases, leaving other factors constant.
  - changing random seeds and how this affects learning.
  - how averages are used in experimental trials.
  - how environmental characteristics change outcomes.
  - differences by stochastic environments.

Starting with the [OpenAI/baselines](https://github.com/openai/baselines) repository, they attempted to reproduce reported results from the corresponding papers for each algorithm (both the original papers as well as results from the broader machine-learning community). They break down their findings in the following sections:

---

### Hyperparameters and Network Architecture
(Actually two different sections)

They note that hyperparameters and network architecture decisions have a huge effect on convergence and reward outcome, but are often never reported in papers.

### Reward scale
Mostly in reference to linear scaling (usually something like $\hat{r} = r \sigma | _{\sigma=0.1}$), which is similar in nature to reward clipping (usually to $[0,1]$).

The intuition of reward scaling is that, if the envirionment has a large and sparse output, then saturation of invalid reward and inefficient learning can result. To this effect reward scaling can compress the $\hat{r}$ state. In the wild, this can have a large effect but the results are inconsistent across different environments and across different scaling values.

There is also reward rescaling -- this gets affected by layer normalization. Some enviroments have untuned reward scales, which implies that there is no way to use this in a principled fashion. The most promising technique here is Adaptive Rescaling ([Hasselt, et al., 2016][hasselt], [pdf][hasselt-pdf]), with normalized stochastic gradient descient, which might be more resilient to varying environments.


### Random Seeds and Trials

Should you randomize your seeds? Yes (definitely). Should you average with an improper number of trials? Yes, otherwise you will wind up with distorted results.

Unfortunately, it is common to _only select the top N-seeds_ or _only select the top N-trials_ when reporting averages in the deep reinforcement learning community. This sounds like one of the most preverse things you can do when submitting results in papers, but the problem is a cultural one. There is no solution here, other than to share plots of all averages, plots with clustered averages (by differing behaviours / distributions), and by plotting all individual trials.

### Enviroment Choices

Another common occurance in papers is to selectively choose "novel" environments for algorithms to succeed in. It is important to showcase as many environments as possible -- showing where an algorithm succeeds most, as well as where it fails to succeed. Another suggestion that Henderseen, et al. make is to pair local optima with examples, to expose any reward hacking that a model converges on (in essence, where quantitative results do not match qualitative expectations). This should be done in
the form of gifs or videos, where relevant (perhaps audio or text depending on the problem domain). As an aside, this reminds me of the idea that most non-RL models should actually train on a 3-way training data split: one train set, one test set, and one holdout set which is used purely for a qualitative analysis.

### Codebases

The last comment that is made in these sections is that codebases matter. The same algorithms written twice can be considered two different models, even when holding hyperparameters the same. While I don't recall if this is explicitly regarding the use of multiple frameworks or not, I think it is worth mentioning that results will vary depending on different hardware-architectures and how machine-level imprecision is handled. For this reason, it is very important to document and package papers
with codebases.

---

## Reporting Evaluation Metrics

Common malpractice in DRL reporting includes only showcasing the max-reward over time, a single fixed random seed with the best result, or the average cumulative reward. All of these should should be considered insufficient (see "Random Seeds and Trials").

Improvements would include confidence bounds, power analysis, and significance on the reward (or comparable metric). For reporting confidence intervals at each time step see [Kohavi, et al. 1995][kohavi], [Bouckaert and Frank, 2004][frank]([pdf][frank-pdf]), [Nadeau and Bengio, 2000][nadeau] ([pdf][nadeau-pdf]). This would include bootstrapping with samples to the 95% confidence level and when the confidence interval is too large, more trials should be run.

In the supervised learning domain k-folded t-tests and corrected resampled t-tests, among others, have been used which give clear guidance with respect to a model's performance. Henderson, et al attempted to apply this to the DRL field without success. Instead, a simple 2-sample t-test produced qualitatively clear results, using sorted final evaluation returns over a fixed number of random trials with different random seeds. Kolomogorov-Smirnov tests have been explored by [Wilcox, 2005][wilcox], and seem useful in differentiating distributions. Bootstrapped percent differences with a 95% CI also seem successful.

One interesting result of their findings is that the difference between DDPG and ACKTR is not statistically significant.

---

## Conclusion

Track **_every single goddamn thing_** and don't aggregate the results until you really have to. Even then, you should report all individual trials so that others can investigate further. Make all of your code available, and use one of the recommended statistical tests to quantify your findings! Don't be afraid to have an appendex that takes up the majority, or close to the entirity, of your paper or reported results. Record all tangentially important information you can get your hands on!

Basically, this echos reflections of recent internet ramblings by [Alex Irpan][alexirpan] ([perma][alexirpan-perma]) and [Matthew Rahtz][amidfish] ([perma][amidfish-perma]).

[drltm]: https://arxiv.org/pdf/1709.06560.pdf

[hasselt]: https://papers.nips.cc/paper/6076-learning-values-across-many-orders-of-magnitude
[hasselt-pdf]: http://papers.nips.cc/paper/6076-learning-values-across-many-orders-of-magnitude.pdf

[wagstaff]: https://arxiv.org/abs/1206.4656
[wagstaff-pdf]: https://www.icml.cc/2012/papers/298.pdf
[stodden]: https://www.crcpress.com/Implementing-Reproducible-Research/Stodden-Leisch-Peng/p/book/9781466561595
[stodden-pdf]: https://www.jstatsoft.org/article/view/v061b02/v61b02.pdf
[stodden-pdf-perma]: https://perma.cc/K8Z7-E5XT

[frank]:https://link.springer.com/chapter/10.1007/978-3-540-24775-3_3
[frank-pdf]:https://www.cs.waikato.ac.nz/~eibe/pubs/bouckaert_and_frank.pdf
[kohavi]: https://dl.acm.org/citation.cfm?id=1643047
[nadeau]: https://link.springer.com/article/10.1023%2FA%3A1024068626366
[nadeau-pdf]:https://papers.nips.cc/paper/1661-inference-for-the-generalization-error.pdf
[wilcox]: https://onlinelibrary.wiley.com/doi/full/10.1002/0470011815.b2a15064

[alexirpan]: https://www.alexirpan.com/2018/02/14/rl-hard.html
[alexirpan-perma]: https://perma.cc/KZS6-KLQE
[amidfish]: http://amid.fish/reproducing-deep-rl
[amidfish-perma]: https://perma.cc/V6N3-B9WY


