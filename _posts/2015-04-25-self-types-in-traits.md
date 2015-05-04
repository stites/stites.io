---
layout: post
title: Self Types in Traits
---

<p class="meta">25 April 2015 - Mountain View</p>


{{ page.title }}
===========

Last time we learned that a scala trait might extend a class. Well — that might offer up a little confusion when the superclass for any of those mixins references when looking at self references. For this reason, scala has _self types_.

When a trait starts with `this:_Type_ =>` then it can only be mixed into a subclass of the given `_Type_`. This is similar to a trait with a supertype, when invocations to `super` were post-fixed with the supertype in brackets (`super[_SuperType_].method`).

The self type can also be used to specify a structural type — a type that specifies methods a class has without specifying the class itself. Both will compile:

    trait Bar { def bar(x: String) }

    trait Foo {
      this: Bar =>
      def log(y: String) {}
    }

    trait Foo {
      this: { def bar(x: String) } =>
      def log(y: String) {}
    }
