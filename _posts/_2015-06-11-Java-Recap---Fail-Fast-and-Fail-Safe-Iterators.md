---
layout: post
title: Java Recap - Fail Fast and Fail Safe Iterators
---

{{ page.title }}
================

<p class="meta">11 June 2015 - San Francisco, CA</p>

This has to do with concurrency. Pre-jdk1.4 fail fast iterators were the primary
way of going about things, but now fail safe iterators are all the rage. The
difference between the two have to do with concurrent modification.

In a fail fast iterator, you have direct access to the collection you iterate over.
This means that your iterator may come across error and will throw them as soon
as they occur.

In a fail safe iterator, you might be working with a copy or snapshot of the
collection. This is resource-intensive to implement, but makes things safer and
implies that you will have more features at your disposal in the iterator's api.
In the latter case there will be no errors, as the name implies.
