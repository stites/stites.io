---
layout: post
title: Least Interesting Data (LID) caches
---

Techniques like experience replay, sampled traces, and what
I am expecting to read about in [the sample-efficent Reactor
architecture][reactor], all seem to be targeting the idea that we want
to efficiently reuse experience for efficent and stable learning.
Also of note is that DQN and DRQN architectures use a dedicated memory
buffer which is randomly sampled for faster learning.

So a quick thought that came to me: can we do better than random
sampling if we are occasionally antifragile? In essence, what if we
sample intelligently: mostly attempting to have uniformly-distributed
samples but occasionally sampling from the least-frequent or
most-advesarial data points.

While this would require a bit of research, one of the first ways we
could try to implement antifragile sampling would be to maintain a
cache's probability distribution and evict data according to some "Least
Interesting Data" policy (a riff off of the LRU policy).

[reactor]:
