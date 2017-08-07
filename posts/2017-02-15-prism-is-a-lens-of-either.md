---
layout: post
title: Prism is like a Lens of Either
---

{{ page.title }}
================

My coworker quickly explained one of the more esoteric components (Prism)
quite susinctly today:

> Prism is a `Lens s a` where `a` may or may not be in `s`. A good
> example of this is that Prism is like `Lens (Either a b) a`.

I'm not going to say that this is a hard and fast rule to live by, but I
hope it'll provide a good starting point for some future Lens studies.



