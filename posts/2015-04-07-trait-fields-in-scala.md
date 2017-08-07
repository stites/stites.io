---
layout: post
title: Trait Fields in Scala
---

<p class="meta">07 April 2015 - Reykjavik, Iceland</p>

I mentioned in an earlier post that methods declared, but not defined, are implicitly defined as abstract:

    trait Foo {
      def bar(x: String)     // abstract definition
      def baz(y: String) {}  // concrete definition
    }

The same goes with fields:

    trait Foo {
      val bar:Int   // abstract field
      val baz = 10  // concrete field
    }

When a class inherits from a trait, these fields are placed into the subclass - _not_ inherited. Note that the JVM itself only allows for one superclass, so traits can't be inherited in the same way - hence this distinction.

It's also worth noting that, when you are dealing with traits which contain abstract fields, you must override the field in the class itself - just like you must with methods. Some syntactic sugar includes that you do not need to add an `override` modifier in your class in order to initiate abstract fields.