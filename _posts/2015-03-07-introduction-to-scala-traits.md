---
layout: post
title: Introduction to Scala Traits
---

<p class="meta">07 March 2015 - Mountain View</p>

I've been digging into scala more and more. Inspired by Horstmann's _Scala for_
_the Impatient_, I'm writing a series of posts on parts of Scala I find
interesting. Some of the examples I show will come directly from Horstmann,
but only because they're pretty great examples. I highly recommend the book.

{{ page.title }}
============================

Scala traits are pretty cool. They're basically the same as interfaces in Java.
Except for the fact that they're a lot more flexible than interfaces and they've
been made to address the multiple inheritence problem which is missing in Java.
Austin (coworker) says that it's the closest thing he's seen to multiple
inherience done in C++.

Lets start with why multiple inheritence done in Java. Mutliple inheritence is
a powerful feature and is simple to think about when you have completely different
classes — but things get crazy when you start to inherit from similar ones.
Properties overlap and, soon, you have seven different definitions of an
'overridden' function.

This is classically defined in The Diamond Problem (or the 'deadly diamond of
death' - wikipedia):
![Diamond Inheritance](https://upload.wikimedia.org/wikipedia/commons/thumb/8/8e/Diamond_inheritance.svg/440px-Diamond_inheritance.svg.png)
_from wikipedia_

In Java, they ignore this problem completely. You can only inherit from one
abstract class — although you can bind your class to any number of interfaces as
you'd like. In C++, you would have to redefine any base class defintions to clear
things up. In Scala, you would use traits.

As I mentioned, traits are a lot like java interfaces. In fact you can use them in
the exact same way. However you can declare definitions, properties, or methods in
both the concrete and the abstract. Even alongside each other!

Methods declared, but not defined are implicitly defined as abstract:

    trait Foo {
      def bar(x: String)     // abstract definition
      def baz(y: String) {}  // concrete definition
    }

With regards to mutliple inheritance, you can simply chain traits with the `with`
modifier:

    class X extends TraitA with TraitB with TraitC { /* ... */ }

Note that the compiler reads this as `class X` extending `TraitA with TraitB with
TraitC`. Traits will inherit from eachother in FILO (first in, last out) fashion.
So if - in the above example - `TraitA`, `TraitB`, and `TraitC` all override the
method definition `def bar(x:String) {}` in TraitA, then class X will inherit bar
from TraitC.

Some other notes on scala's traits:

  + you don't need to declare an `override` on a class when implimenting an
  abstract definition.
  + traits don't have to be abstract.
  + think of traits as mix-ins.

Finally keep in mind that, when refactoring traits with concrete behaviour, all classes inheriting from that trait must be recompiled.

