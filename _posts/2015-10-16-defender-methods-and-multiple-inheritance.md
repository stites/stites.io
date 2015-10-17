---
layout: post
title: Defender methods and multiple inheritance
---

{{ page.title }}
================

<p class="meta">09 Oct 2013 - San Francisco, CA</p>

We've started the shift to Java 8, over at Bina, and I have been spearheading the a
lot of the fresh code. As someone who thinks and reads up on Haskell almost daily at
this point. I'd like to wrap up some of my older, backlogged blog posts regarding the
jvm and start shifting focus to things I'm more interested in.

---

As far as I've gone with java 8, a number of features have proven invaluable in
keeping our code clean and flexible include some of the new interface functionality:
defender methods, static methods, and functional interfaces. I'm just going to cover
defender methods for today, covering this nice OO-feature that eases a lot of
single-class inheritance pain, and circle back to talking about Haskell typeclasses
and how they relate.

#### Java finally gets a mixin, takes a stab at the diamond problem

I don't believe mixins have really been all that simple to implement in a <8 Java
world. In my searches for the history of this, I've come across [a couple][hack1] of
[hacks][hack2], as well as a more elegant solution (aspect-oriented programming)
which is not likely to be picked up by my company anytime in the immediate future.

Enter defender methods - also known as *Virtual extension methods*, or just "default
methods". Interestingly enough, almost no one seems to advocate for this feature's
use in live production (at least not from what I can see). My co-workers are rightly
a little skeptical, but I - personally - think that the ability to introduce multiple
inheritance to our code is a "Good Thing". So what does happen if you come across the
diamond problem through interface-dependent inheritance?

Java's approach is to use explicit qualification - thank god it wasn't lexographic
ordering. If the compiler happens across an ambiguous method name *BOOM*, compiler
error. Specifically `class Foo inherits unrelated defaults for bar() from types X and
Y`. You'll have to override and explicitly declare which method you want to use:
usually referencing the method like `interfaceName.super.methodName()`. Of course you
may also want to roll your own implementation at this point. While this might seem
onerously verbose, it's right at home with java's grammar and, I think, a much better
solution than implicit solution.

To give a little more insight into how we put this into practice, I must admit that
we do only use this feature in test. It's been quite nice to modularize redundancies
in our API tests by splitting up the boilerplate on a per-http method bases, have
them inherit shared basic methods, and - finally - implement multiple http method
interfaces for a feature's test class. I would venture to state that, since this
feature is on the newer side of java and could have wide-ranging implications -- it
may be wise to leave it there for the time-being.

---

So long jvm!

[hack1]:http://stackoverflow.com/questions/587458/implement-mixin-in-java
[hack2]:http://stackoverflow.com/questions/263121/java-traits-or-mixins-pattern

