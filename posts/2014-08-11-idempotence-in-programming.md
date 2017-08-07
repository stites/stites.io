---
layout: post
title: Idempotence in Programming
---

<p class="meta">08 August 2014 - Redwood City, CA</p>

In mathematics, the concept of idempotence implies that an operation can be
executed any number of times and still yield the same result as if it was only
run once. `f(f(x)) = f(x)`.

Programming-wise, the word means that some method or function which mutates a
variable  has no additional effect on the variable if it is called on it more
than once, given the same input parameters. For instance, if you initialize a
singleton, then try to reinitialize a new instance of that singleton, you will
recieve the same instance.

It's fun to see the similar vocablary sping up in programming since I came from
a theory-heavy math/stats/physics background with less programming. Excited to
see more of this!
