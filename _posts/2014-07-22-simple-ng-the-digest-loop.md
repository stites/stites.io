---
layout: post
title: simple-ng: rebuilding angular
---

{{ page.title }}
================

<p class="meta">22 July 2014 - San Francisco, CA</p>

Of late, I've been rebuilding angular by following Tero Parviainen's
[Build your own Angularjs][ng]. After finishing up work on the digest loop, I
figure it would be best to go through what I've learned so far.

#### What is the digest loop?

It's really beautifully simple. The digest loop is simply an eventing system
that maintains angular's scopes for databinding. This is, in my opinion, one of
the building blocks of what it means to build something in 'the angular way.'

This statement begs a couple more questions which I'll answer very quickly:

##### what is a `scope`?

Just an object. It holds data for databinding.

##### what is `$apply`?

Invoking a function in the digest loop.

##### what is `$watch`?

The equivalent of setting up an event listener, only using the digest loop.

##### what is `$eval`?

Running a function once in the digest loop.

##### what is `$evalAsync`?

Running a function once in the digest loop at the end of a single loop.

#### So why is the digest loop so special?

In my opinion, the digest loop has the special sauce of chaining. If you...

...getting booted from this coffeeshop. moar later




[ng]: http://teropa.info/build-your-own-angular
