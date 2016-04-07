---
layout: post
title: Eq your errors
---

{{ page.title }}
================

When building out error types in Haskell, we _must_ derive `Eq` or the compiler can't pattern-match on them.

One instance of this is for using `case` expressions where the constructor is the
condition. Thus, if the error's constructor is the condition, we must have a way to
compare constructors. We could, technically, build our own custom `Eq` definition,
but for something as simple as errors, why would you want to?

