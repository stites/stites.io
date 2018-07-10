---
layout: post
title: Deep Reinforcement Learning that Matters (1709.06560)
---

A quick write up of some notes on [Deep Reinforcement Learning that Matters](https://arxiv.org/pdf/1709.06560.pdf) that I took on the plane.

So the paper itself focuses on Model-Free Policy Gradient methods in continuous environments and is an investigation into how reproducing papers in the Deep Reinforcement Learning space is notoriously difficult. The authors discuss various failure cases that any researcher will be privy to when trying to implement work, and the shortcomings of the majority of authors who follow standard publication practices. Primarily, the key factors in DRL that hinder
reproducability stem from non-determinism and the large data requirements surrounding this class of algorithm.

The authors also reference questions of reproducability and good experimental practice in related fields (Wagstaff '12, Reng '14), and go on to introduce better evaluations methods in benchmarks -- evaluating TRPO (with conjugate gradient descent w/ the KL constraint) and PPO due to their use of constraints and advantage estimation, as well as DDPG and ACKTR (a Kronecker-factored trust region) for their use of actor-critic methods with Monte-Carlo rollouts.

Henderson, et al. set out to investigate issues in the following:
  - the effect of specific hyperparameters, if not tuned properly.
  - differences due to codebases, leaving other factors constant.
  - changing random seeds and how this affects learning.
  - how averages are used in experimental trials.
  - how environmental characteristics change outcomes.
  - differences by stochastic environments.

Starting with the [OpenAI/baselines](https://github.com/openai/baselines) repository, they attempted to reproduce reported results from the corresponding papers for each algorithm (both the original papers as well as results from the broader machine-learning community). They break down their findings in the following sections:

### Hyperparameters and Network Architecture
(Actually two different sections)

They note that hyperparameters and network architecture decisions have a huge effect on convergence and reward outcome, but are often never reported in papers.

### Reward scale
Mostly in reference to linear scaling (usually something like $$\hat{r} = r \sigma | _{\sigma=0.1}$$), which is similar in nature to reward clipping (usually to $$[0,1]$$).

The intuition of reward scaling is that, if the envirionment has a large and sparse output, then saturation of invalid reward and inefficient learning can result. To this effect reward scaling can compress the $$\hat{r}$$ state. In the wild, this can have a large effect but the results are inconsistent across different environments and across different scaling values.

There is also reward rescaling -- this gets affected by layer normalization. Some enviroments have untuned reward scales, which implies that there is no way to use this in a principled fashion. The most promising technique here is Adaptive Rescaling (Hassalt, et al -- 2016), with normalized stochastic gradient descient, which might be more resilient to varying environments.

