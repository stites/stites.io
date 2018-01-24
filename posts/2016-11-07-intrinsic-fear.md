---
layout: post
title: Intrinsic Fear (paper)
---

Cleverly posted before the US election. This paper talks about the
introduction of "Intrinsic Fear" for DRL algorithms.

The idea here is that DRL is being used in more and more life-or-death
scenarios, but function approximation will make a RL algorithm to forget an
infrequent occurence and repeat the mistake — turning into the Sisyphean Curse.

They basically introduce a supervised "danger model" alongside their DQN
implementation which models the analogy to "catastrophic failure" — I'm thinking
something to the effect of "black swans." This danger model then becomes an
extra source of reward (or, rather, cost). Check it out at: "[Combating
Reinforcement Learning's Sisyphean Curse with Intrinsic
Fear](https://arxiv.org/abs/1611.01211)"

