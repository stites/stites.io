---
layout: post
title: Java 8 interface fun: default methods
---

{{ page.title }}
================

<p class="meta">09 Oct 2013 - San Francisco, CA</p>

We've started the shift to Java 8, over at Bina, and I have been spearheading the a
lot of the fresh code.

A few features that have proven invaluable in keeping our code clean and flexible
include some of the new interface functionality: default methods, static methods, and
functional interfaces. I'm just going to cover default methods for today, due to some
time constraints.

#### Java finally gets a mixin, takes a stab at the diamond problem

I don't believe mixins have really been all that simple to implement in a <8 Java
world. In my searches for the history of this, I've come across [a couple][hack1]
of [hacks][hack2], as well as a more elegant solution (aspect-oriented programming)
which is not likely to be picked up by my company anytime in the immediate future.

Enter default methods. I've seen some critiques about how this muddles the
interface's "interface" : ) - which I can definitely see. But Interfaces are a lot

given the history and constraints of Java - I'll take some progress over no progress.



is ank interface width one abstract method, which means that - in the world of Java 8 - we can substitute its definition with a lambda expression. 

[hack1]:http://stackoverflow.com/questions/587458/implement-mixin-in-java
[hack2]:http://stackoverflow.com/questions/263121/java-traits-or-mixins-pattern

