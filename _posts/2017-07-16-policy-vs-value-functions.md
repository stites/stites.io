---
layout: post
title: Policy vs Value functions
---

{{ page.title }}
================

Usually, policy _and_ value functions work best, but most of the time
finding a policy is preferred over finding a value function. Of course
this it dependent on your use case -- it might not be possible to find
a policy as easily as a value function, or finding a policy might
over-simplify the situation. Part of what makes policies nice is the
fact that they can be represented in a more compact manner. With a
policy you get something as concrete as: "Knights can move in L-shape,
here is a distribution to make your choice" -- paired with more
features, you get complex strategies which allow agents to win at a
human level of skill.

Conversely, a value function says, "your position is (A,4) with value X, let's
compute the maximum possible value we can squeeze out of all
options." Furthermore, the policy used with a value function is
classically a maximum -- something which can be very costly in high
dimensions of state- or state-action- spaces. The maximizing actually
turns out to be very interesting (as points of friction always tend to
be): computing the distribution to argmax over is your policy, but if we
move to a policy-based learner we just comput that directly.

As an added benefit you also you wind up with better convergence
properties. Value functions solve for estimated value and advantage,
but it's possible that the hyperparameters might chatter, or oscillate,
for non-linear approxmiators since value can sometimes be at odds with
advantage.  So just throwing a neural network at your problem which
doesn't live on the gym might fall apart for no reason at all, and also
motivates dueling networks (ie: maintaining a network for value and a
network for advantage).

---

Policy gradients are pretty good at finding stochastic policies and it's
what they are most famous for up until DDPGs. Deterministic policies
have become pretty popular of late as well because of Deepmind's
research into learning that compatible value functions can speed up
learning (ie: if the value function and the policy gradient use the same
hyperparameters, then a critic can directly influence an
actor). Keep in mind, however, that deterministic policies aren't
always advisable. Imagine writing a deterministic policy for
rock-paper-sissors: "since she threw rock last time, I'll throw paper
this time." As soon as your opponent figures that out, you're done!

In the wild, policy gradients are sometimes very noisy and taking
expectations of noisy gradients can be very hard to estimate. If
your model has a high accuracy, but noise naturally exists in your
problem, then your variance will also be high. So instead it's better
to start off with a deterministic policy, and then adjust it to be more
stochastic. The _natural policy gradient_ finds the deterministic policy
(but only works for continuous actions). How it works is that it, when
we get the gradient of the Q-function the critic can give the entire
gradient to the policy and just say, "if you adjusted to this better
gradient, you'd win." This is allowed since the critic doesn't just
have the critique, but also the "true answer" since the Q-function
is compatible. In sort, we are just updating actor parameters in the
direction of critic parameters. See Natural Actor Critics for more.

---

A few other, possibly incoherent, notes:

- You're only garunteed a local maximum with policy gradients
- On top of local maximums, policies can be more costly to calculate than value functions "in the small" -- they have higher variance for non-linear approximators (ie: "hard real-world" models). Basically there's no such thing as a silver bullet.
- For non-linear approximators there's a lot of research to get better stability: dueling networks (see above), double-learning, fixed Q-targets (see DQNs).
- Actor-Critic methods work because we using policy gradients, which are slow, but then we speeding up the process not by looking at current value, but by looking at _future_ values, estimated by the critic.
- Using "compatible function approximation (which have no bias)" for value approximation makes garuntees around our value function converging on the true value function and solves the problem of a bad critic
- ...but compatible function approximators turn out to always be true only if the features are _exactly_ the score function. So there's a bit of a chicken-and-egg problem.
- Eligibility traces: after we build up a history, we want to train on scores that are the largest, most frequent, and most recent. is an eligibility over our scores (not our states). For a policy gradient it is analgous to the value function (search-replace "gradient" with "score").
- there are other ways of backpropagating for things like random forests (maybe?) check out simplex method for random forest optimization.
- build out different types of ACs:
  + Q Actor-Critic
  + Advantage Actor-Critic (A2C)
  + TD Actor-Critic
  + TD(lmda) Actor-Critic
  + Natural Actor-Critic (NAC)

