---
layout: post
title: Symbol Acquisition for Probabilistic High-Level Planning
---

{{ page.title }}
================

A framework that allows an agent to learn its symbolic representation of a
low-level continuous environment. Proposal symbols are formalized as names for
probability distributions, providing a natural means with dealing with uncertain
representation and probabilistic plans.

+ building high-level planning with low-level control (symbolic reasoning) for
  goal-driven behavior is hard and an active field of research.

+ Konidaris showed how to automatically construct symbolic representations
  suitable for planning in a high-dimensional, continuous domain.

  - the key result was that symbols required to determine the feasibility of a
    plan are directly determined by characterstics of actions available to an
    agent. This is the product of the world??? We find this with a SMDP
    (Semi-Markov decision process)

Set based formulation cannot deal with learned sets which may not be exact.
- ie: if we have only observed partial sets, something pretty common, this falls
  apart.

What is a Semi-Markov Decision Process?

(S, O, R, P, gamma) for a fully-observable, continuous-state, semi-MDP:
- S contained in R^n is n-dimensional, continuous state-space
- O(s) is a finite set of temporarily extended actions (options)
- R(s', tau | s, o) is a reward when executing option o in O(s) and arriving in
  state s' after tau steps
- P(s', tau | s, o) is a PDF describing the probability of arriving in state s'
  in S, tau steps after executing option o in O(s).
- gamma is a discount factor

An option has three parts:
- an option policy, ∏°, which is executed when the option is invoked
- an initiation set, I° = {s | o in O(s)}, what states the option may be executed in
- termination condition beta°(s) —> [0,1] which is the probability that the
  option execution terminates when reaching state s.

(1), (2) and a transition set make an _option model_, but we assume that an
agent doesn't have access to the transition set.

What is probabilistic high-level planning?

Most simple: set-theoretic representation
A planning domain is a set of propositional symbols P = {p1..pn} and actions.
Each action is a precondition and a set of possible outcomes, plus the
probability of each condition happening.












