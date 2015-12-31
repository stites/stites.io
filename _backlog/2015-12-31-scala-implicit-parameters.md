---
layout: post
title: Scala: Implicit Parameters
---

{{ page.title }}
================

<p class="meta">Dec 31, 2015 - San Francisco, CA</p>

In working on CSSR, I've stumbled upon scala's dreaded `implicit` keyword,
specifically as a parameter, that I keep hearing bad things through various people
and communities. Problems are solved by gaining knowledge and understanding, however,
so let's fill some of the gap by adding an incremental amount of knowledge to the
register.

------------------------------------------------------------

From my investigation with scala docs, under the post [Implicit Parameters][scaladocs], I can safely state the following:
  + implicit parameters are used in the same way as normal parameters
  + if a method misses an argument on invocation, and this argument is implicit, a
    lookup is required to "automatically provide" this argument.
  + the above lookup seems to be how we wind up in implicit hell.

Arguments allowed to be passed to an implicit parameter are one of two:
  + identifiers without prefixes: so things that do not live on objects, or are not
    namespaced.
  + members of companion modules from the implicit parameter's type that are labeled
    as implicit -- need some more context for this one.

Okay... that seems to be all in the documentation. It's still pretty lacking -- among
other things, the scala docs are missing in interaction between multiple packages
importing implicits within a project. A couple of things that I've heard from "around
the block" include that implicit imports are lexigraphically loaded, so be careful of
that. Other that that I'll have to investigate further at another point in time.

I'll cook up some copy-pasta from the scala docs, below, to give you an example of
this simplistic, single-object version. To my understanding, however, there are
multiple types of implicits -- implicit classes and implicit parameters -- and someone aptly mentioned that the scala docs conflate the two. So be wary of that.

    abstract class SemiGroup[A] {
      def add(x: A, y: A): A
    }
    abstract class Monoid[A] extends SemiGroup[A] {
      def unit: A
    }
    object ImplicitTest extends App {
      implicit object StringMonoid extends Monoid[String] {
        def add(x: String, y: String): String = x concat y
        def unit: String = ""
      }
      implicit object IntMonoid extends Monoid[Int] {
        def add(x: Int, y: Int): Int = x + y
        def unit: Int = 0
      }
      def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
        if (xs.isEmpty) m.unit
        else m.add(xs.head, sum(xs.tail))
      println(sum(List(1, 2, 3)))
      println(sum(List("a", "b", "c")))
    }

Apologies about this post. I thought it would at least put us in a small step
forward, but instead it turned out to add no value to the conversation. I'll backlog
it for now and see what I can do about it later.

[scaladocs]: http://docs.scala-lang.org/tutorials/tour/implicit-parameters.html

