---
layout: post
title: Multi-step bootstrapping thoughts
---

{{ page.title }}
================

While eligability traces are usually associated with multi-step
bootstrapping, there is a lot to be said for multi-step bootstrapping
techniques on their own. These techniques unify temporal-difference
learning and monte-carlo methods. Often performing better in the
intermediate steps than TD or MC, the two extremes of the multi-step
variants. Quoting Barto and Sutton, "another way of looking at
multi-step methods is that they free you from the tyranny of the time
step."

This is incredibly cool because we now have the ability to extend our
horizon at any point in time and no longer have each tick of the clock
hord its rewards. That said it's usually preferred to have off-policy
methods instead of on-policy methods, and if we naively extend a TD-walk
through time, we wind up only samping our target policy.


    Fig 1: ASCII art of three n-step TD backup diagrams

        *-->O-->*-->O-->*                       2-step

        *-->O-->*-->O-->*-->O-->*               3-step

        *-->O-->*-->O-->*-->O-->*-->O-->[x]     terminated infinite-step / MC

                                    ,->*
        *-->O-->*-->O-->  ...  *-->O-->*        n-step expected sarsa
                                    `->*

But look at the samping which happens (each `*-->O`): it requires
following our target policy. How can we separate this? This introduces
the notion of importance samping. Where in we actually evaluate each
sample _twice_: one under our target policy, and one under our
behavioral policy. Then we can weight each sample by
`for all k . product $ pi(A|S) / mu(A|S)`. That said, we aren't really
following a behavioral policy, just using it for more informed
updates. We can do better. This is where Q(σ) comes in

> Note: If you are worried about the computational efficiency of
> the off-policy evaluation, you should note that it is probably
> inevitable that it will be slower than the on-policy as the
> behavioral policy will model a less-relevant environment than your
> target policy. Possible improvements are cited in B&S to include:
> the Autostep method (Mahmood et al, 2012), invariant updates of
> Karampatziakis and Langford (2010), the usage technique (Mahmood and
> Sutton, 2015).

