---
layout: post
title: Why Causal States?
---

{{ page.title }}
================

<p class="meta">10 Oct 2015 - San Francisco, CA</p>

I've started work on cleaning up some loose ends from Cosma and Kristi's CSSR (Causal
State Splitting Reconstruction) paper, "Blind Construction of Optimal Nonlinear
Recursive Predictors for Discrete Sequences" - [arxiv:cs.LG/0406011][cssr]. In this
post I want to prime you with the environment of the algorithm at a high-level, as
well as go over some possible motivating examples of what makes CSSR interesting.

----

Let's say that we are examining some finite stream of data that is seemingly random,
and want to find the hidden state which determines how this stream is being output.
We know a couple of things about this stream - for one, we have some finite length of
it which we can train on. We also know the fundamental building blocks which make up
this stream, this we call the "alphabet" - we are dealing with serializable inputs
after all : ), and the longest possible sequence that could be considered a state.

Given these bare-boned constraints, we're looking to build a predictive model which
is next-step sufficient in its predictions. In essence, we are looking for a model
which can accurately predict the next step, given a sequence in this stream.

----

So that's the abstract characterization. Let's dream up an example that is a little
more fun:

One of my first and favorite examples of how to explain a Hidden Markov Model is by
telling the story of a kid at Disneyland who intentionally gets seperated by her
parents so that she can explore the park on her own. In this world, both you and I
are Elsie's parents and - god damnit we are worried! We're not bad parents, I swear,
we've been walking around with Elsie all day and exploring where _she_ wants to go:
I guess that isn't enough! Luckily, DisneylandSF is a lot cheaper than all the other
ones and there are only a few places she could be. Also, we know that Evil Megacorp
Disney makes all their parks in the same way, directing the flow of children towards
snares of cotton candy, costumed actors, and unsupervised go-kart rides - how is that
safe?!

If we want to find Elsie the most simple way of going about it would be to think
of where we saw her last, consider all the possible places in the park, and - based
on where we had travelled with her before (I mean, c'mon, we're just as susceptable
to cotton candy, actors, and gokarts) - guess at where she may be now. We would be
considering how all of our previous "states", locations in the park, caused us to
move in a certain direction towards another "state" in the the park.

Using locations in the park as our "alphabet," given our full history of walking
through DisneylandSF as our "stream," and only allowing the longest possible sequence
of movements to be the time we have lost Elsie for - CSSR attempts to reconstruct the
smallest sequence nessecary to predict where we might end up and, ultimately, where
we'll find Elsie.

----

One of the most fascinating parts of this research is a look into some of the deeper
waters of computer science. Causal State Theory itself, which is somewhat difficult
to find explicit resources on, happens to also allow us to build epsilon machines: a
mathematical concept that exemplifies Occam's Razor. However this fact only recently
surfaced, so I'll have to go over what these are at a later point.

Question brought up while writing this post: how do we deal with an evolving or
conditional alphabet?

[em]:http://csc.ucdavis.edu/~chaos/courses/ncaso/Projects2012/Sanderson/KillingAndCollapsing.pdf
[cst]:http://csc.ucdavis.edu/~cmg/compmech/tutorials/cstgcm.pdf
[cssr]:http://arxiv.org/abs/cs.LG/0406011
[desc]:http://nicolas.brodu.net/en/recherche/decisional_states/index.html
[desc-em]:http://arxiv.org/abs/0902.0600
[em-2]:http://www.milegu.org/research-notes/epsilon-machines-a-occams-mathematical-razor/

