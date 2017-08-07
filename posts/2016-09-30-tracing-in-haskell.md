---
layout: post
title: tracing in haskell
---

{{ page.title }}
================

Today I learned about `traceM` and `traceShowM`. First off: wow! debugging is
so much cleaner now! Secondly, this is important because with `trace` you are at
the will of lazy evaluation - so if you are running things monadically, you now
have the ability to garuntee more ordering of your statements. Talk about cool.

