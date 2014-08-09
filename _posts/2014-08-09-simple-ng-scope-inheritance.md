---
layout: post
title: Rebuilding Angular in simpleNg - Scope Inheritance
---

{{ page.title }}
================

<p class="meta">09 August 2014 - San Francisco, CA</p>

This is the second post I've written from rebuilding angular under the guidance
of Tero Parviainen's [Build your own Angularjs][ng]. Last time, I went through
a quick-and-dirty explaination of the digest loop and today it's scope
inheritance. If you don't settle for perculated wisdom, like myself, I'd
recommend [buying the book][ng]. However if all you want is the high level to
get that understanding in, then hopefully these posts should be good enough.

#### Scope Inheritance: what's the big deal?

So lets put this out there first: Scope inheritance is the exact same as
javascipt's prototype chaining. Yes, there are some extra embelishments added
in the mix, but for the most part they are the same. When you create a new
child scope, imagine as if you are creating an object in an object - because you
are.

Attribute shadowing is a direct consequence of this prototype chaining.
mainly, that inheritence goes down the chain, so a parent can't read properties
of it's childen, but children can see properties of its parents. The 'shadowing'
comes into play when you name a child variable the same as a parent's variable.
Then the child has a 'shadowed' the parent and two instances of this variable
exist in the prototype chain. As a result, there is no interference between
child and parent.

If you don't want to shadow a variable, however, you can always pass by
reference in the prototype chain by manipulating a parent-level object. Imagine
the following:

    parent.user = {name: 'Joe'};
    child.user.name = 'Jill';

    expect(child.user.name).toBe('Jill');
    expect(parent.user.name).toBe('Jill');

this way we aren't writing anything on the child scope, we are _reading_ the
parent scope's `user` then accessing it's `name`. This is part of the _Dot Rule_
see [this video][vid].

### The embelishments

We don't actually want digests to work with javascript prototype chaining,
though. If we depend on just prototype chains, we would inherit the rootscope's
`$$watchers` and we'd executed all of them when we run `$digest` on a child
scope. Seems like overkill.

What we really want to do is execute all the watches based on _scope heirarchy_.
So when we call `$digest` on a node, then it `$digest`s that node and its
children. Not siblings or parents. This requires shadowing the `$$watchers`
array and recursively checking children's watches.

### But what if we want access to the `$$rootscope`?

Well, that's why `$apply` triggers a digest from the rootscope. That's why it's
the recommended way of injecting external code into angular. However, knowing
this, if you know which scope you want to run your code in, you can be more
specific and make your code more optimized by using `$digest`, or some other
method. Just not `$evalAsync` since it schedules a roop scope digest as well.

### Why isolate a scope if digests don't interfere?

Sometimes, you actually want scopes isolated from the parent. This would make
for more modular code and it's why people prefer it in directives. These scopes
still are on the inheritance chain and will be checked on root digests, but they
can't get access to their parent or sibling variables. Parviainen does mention,
however, the following:

    If you’ve used isolated scopes with Angular directives, you’ll know that an
    isolated scope is usually not completely cut off from its parent. However
    this mechanism is not built into scopes. It is part of the implementation
    of directives. We will return to this discussion when we implement isolation
    in directives.

To reiterate: isolation is simple - just make the scope part of the scope
hierarchy, but don't let it inherit from its parent. It's so simple, that it can
be done just by passing a boolean into creating a new scope which makes us
shadow three things: `$$root`, `$$asyncQueue` and `$$postDigestQueue`.

### Doesn't this make destroying scopes complicated?

As the user interacts with your page, it's going to have new controllers and
directive scopes added and removed from the page and it all happens in the
`$$watchers` arrays. So in order to properly manage these scopes, we need to be
able to remove them simply by splicing the `$$children` array of its
parent. Then it won't get digested recursively, and will make all of its watches
dead as the Javascrip garbage collector reclaims the space.

==============

That's it for now! Next up - Angular’s built-in $watchCollection mechanism. As
always, feel free to check out the code I am working on at github:
[stites/simple-ng][repo].

[ng]: http://teropa.info/build-your-own-angular
[repo]: https://github.com/stites/simple-ng
[vid]: https://www.youtube.com/watch?feature=player_detailpage&v=ZhfUv0spHCY#t=1758s
