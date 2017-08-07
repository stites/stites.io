---
layout: post
title: Free your Frees
---

{{ page.title }}
================

I was working on a project of Alex Crough's at the SF Haskell Hack Night which takes the
Free monad to a whole new level. The primary reasoning behind Elision was that
`Free` is actually not "free" enough when it comes to constraints and testing.

An Elision doesn't require that you pass it a functor, and on top of this, it's also
possible to "hot-swap" (or "mock" if you prefer) implementation details while
unit-testing. This allows you to do things like have integration-styled unit tests;
focusing on input and output of your business logic, rather than being forced to
quickcheck every detail in-between.

It feels more like mocking than anything I've seen in haskell so far, so I'm pretty
excited to have been introduced!

For more, [check out the tutorial][0] (a literate haskell file), or the repository
at [crough/elision](https://github.com/crough/elision).

[0]: https://github.com/crough/elision/blob/gh-pages/SansFree.lhs

