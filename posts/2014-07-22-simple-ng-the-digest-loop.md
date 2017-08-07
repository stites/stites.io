---
layout: post
title: Rebuilding Angular in simpleNg
---

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

In my opinion, the digest loop's special sauce is all in the 'loop' part of it's name - and it's really quite simple. This is where the terms dirty checking, and digest cycle come in.

Imagine you have a queue of functions which you need to check for event triggering. Let's represent this as an array of `[f1, f2, f3]`. Now, when the 'event handler' for `f2` executes, it does something in our code which should trigger what `f1` watches. That said, we've already checked `f1` and we're moving on to the last event handler in our queue.

Instead of finishing the queue and letting the browser move on to _it's_ next event loop. We might as well clean up this 'dirty' event queue - the one handled by Angular. I mean, ultimately, if we were to let the browser do it's thing, we would load up a half-baked app. So instead, we'll raise some flag to indicate to our looping function that we're actually not done yet. Our event loop notices that the flag has been raised (that the 'cycle' is 'dirty') and goes in for another round of handling events in the queue. After `f1` gets run, everything is clean again, and we pass control over to the browser.

There's a ton of stuff which happens to ensure that this logic doesn't hit any infinite loops, handles code which isn't naturally part of this digest cycle, and ensures that everything is optimized. I would encourage you to look into [Tero Parviainen's book][ng] if you're interested in more. In the mean time, I'll keep going with these high-level summaries each time I complete another chapter if you are interested and don't have the bandwidth to add another book to read in your daily routine.

Feel free to check out the code I am working on as I move along at my repo on github: [stites/simple-ng][repo].




[ng]: http://teropa.info/build-your-own-angular
[repo]: https://github.com/stites/simple-ng
