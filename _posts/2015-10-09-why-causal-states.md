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
is next-step sufficient in its predictions. In essence we are looking for a
model which can accurately predict the next step, given a sequence in this
stream, without any prior assumptions possible states.

----

So that's the abstract characterization. Let's come up with something a little easier
to consume.

Recently I heard one of the best explanations of Hidden Markov Models to date,
which usurped my former favorite of a child wandering through a theme park. In
doing a bit more digging, I was surprised to discover that this explanation was
actually one a much older example, dating back to 1998.

Let's say we want to predict if tomorrow will be rainy, cloudy, or sunny and we
are going to transition between our measurements every 24 hours. For a Markov
Model, we would want to forecast tomorrow's weather based on what the
probability of today is. So we would want to calculate the probabilities of:
tomorrow being sunny given that today is sunny, tomorrow being rainy given today
is sunny, and so on for all options of what "today" might hold. How do we make
this model a good predictor? Well we'd have to sit outside and wait for, say,
100 days. With this, we have our Markov Model.

Now imagine if we _couldn't_ sit outside everyday and watch the weather. In
fact, we couldn't even see the outside because we have been working in an office
for the past month or so. Hope knowing exactly what the weather is outside seems
impossible, but we _can_ observe things related to the weather to make our
prediction: if our coworkers come in with umbrellas, if everyone brings in
jackets, or if there are a majority of people wearing sunglasses - to name a
few. By estimating the transition probabilities for these observable events, we
can start to model what the underlying state of the weather is outside - our
"hidden" state. Of course, this is called a Hidden Markov Model.

Finally, how should we generate this model? There are many ways we've heard that
we can predict the outside weather but, frankly, today is the first day of being
told we've been locked in the office and we won't be able to leave until the
product has been released. The easiest thing we can say is say that today most
definitely was a "day" since we know that rainy days, sunny days and cloudy days
share at least that in common.

As the days go on, we'll start bucketing out the different possible
umbrella/sunglasses/jacket sequences that might suggest some kind of hidden
state; like that it's sunny outside. In marking down these tallies, we'll also
employ Occam's Razor and assume that a state can only consists of the simplest
sequences to get us there. This really just means that we'll be using some kind
of hypothesis test to ensure that a sequence belongs in a state. If a sequence
doesn't belong in the state that we think it does, we'll try to move it into a
different state's bucket, and if we can't find a state to put it in - still -
we'll move it to an entirely new state!

In order to test our hypotheses regarding when a sequence belongs in a state or
not we're going to do the most robust thing we can to ensure that our model is
correct. Alongside any observed sequence for a given day, we'll also try to add
new information to the history leading up to that day, as well as some one-day
peeking into the future to make sure that we are certain that the state has a
robust collection of sequences.

By going through all sequences in varying lengths, we can attempt to reconstruct
tables with information like "if X people bring umbrellas, then sunglasses, then
jackets today, then tomorrow could be sunny with certainty Y." And we've done so
by assuming the smallest amount of information we can, with no prior
requirements of even knowing what a "state" looks like!

----

That's CSSR! One of the more fun parts of this research is a look into working
with time series data, which I have less experience in, as well as getting
insights into Markov Models, which I have never used before! Causal State
Theory, itself, seems very compelling but is somewhat difficult to find
resources on. It seems that Causal State Theory has more on building epsilon
machines: a mathematical concept that exemplifies Occam's Razor. However this
fact only recently surfaced and I'll have to go over what these are at a later
point.

Question brought up while writing this post: how do we deal with an evolving or
conditional alphabet?

[em]:http://csc.ucdavis.edu/~chaos/courses/ncaso/Projects2012/Sanderson/KillingAndCollapsing.pdf
[cst]:http://csc.ucdavis.edu/~cmg/compmech/tutorials/cstgcm.pdf
[cssr]:http://arxiv.org/abs/cs.LG/0406011
[desc]:http://nicolas.brodu.net/en/recherche/decisional_states/index.html
[desc-em]:http://arxiv.org/abs/0902.0600
[em-2]:http://www.milegu.org/research-notes/epsilon-machines-a-occams-mathematical-razor/
[fba]:https://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm

