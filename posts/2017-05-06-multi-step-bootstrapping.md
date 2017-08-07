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


    Fig 1: ASCII art of four n-step TD backup diagrams

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
updates. We can do better. This is where Q(σ) comes in.

> Note: If you are worried about the computational efficiency of
> the off-policy evaluation, you should note that it is probably
> inevitable that it will be slower than the on-policy as the
> behavioral policy will model a less-relevant environment than your
> target policy. Possible improvements are cited in B&S to include:
> the Autostep method (Mahmood et al, 2012), invariant updates of
> Karampatziakis and Langford (2010), the usage technique (Mahmood and
> Sutton, 2015).

There's an intermediate step between n-step TD and Q(σ) which I've
failed to mention, called n-step tree backups - we'll roll the
explaination of them into Q(σ) here.


    Fig 2: ASCII art of an n-step tree backup diagram

             ,->*    ,->*    ,->*            ,->*
        *-->O-->*-->O-->*-->O-->*  ...  *-->O-->*
             `->*    `->*    `->*            `->*

Under a tree-backup diagram, we use a fully off-policy exploration
technique to walk down through a time series, however as we step, we
also weight any future potential rewards by the conditional probability
of the reward being drawn from the genesis state. While this does work
and allows us to remain fully off-policy, some possible issues may
include the fact that updates will weight larger steps of n with less
importance which makes sense, but we can do better : ).

Q(σ) provides a middle ground between samping (with importance) and
probabalistic tree diagrams. To see how it works, I'll just say that it
is simply an algorithm which alternates between the two techniques above
and I'll leave you with the following backup diagram:


    Fig 3: ASCII art of an 4-step Q(σ=1) backup diagram

             ρ (importance ratio)  ρ
              `.                   |
               '        ,-->*      '        ,-->*
        *--->O--->*--->O--->*--->O--->*--->O--->*
             .         .`-->*    .         .`-->*
             |         |         |         |
            σ=1       σ=0       σ=1       σ=0

