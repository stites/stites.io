---
layout: post
title: Trait Internals
location: San Francisco
---

In the end, Scala needs to translate traits into classes and interfaces for the JVM to be able to handle them.

A trait with only abstract methods is converted into a Java Interface. If a trait has concrete fields, a companion class is created alongside the interface whose static fields hold the trait's values.

    trait ConsoleLogger extends Logger {
      val maxLength = 15
      def log(msg: String) { println(msg) }
    }

turns into

    public interface ConsoleLogger extends Logger {
      public abstract int maxLength();
      public abstract void hash_prefix$maxLength_$eq(int);
      void log(String msg);
    }

    // Companion Class
    public class ConsoleLogger$class {
      public void $init$(ShortLogger self) {
        self.hash_prefix$maxLength_$eq(15);
      }
      public static void log(ConsoleLogger self, String msg) {
        println(msg);
      }
    }

When the trait is mixed into a class, the class will get the appropriate getters and setters. The constructor of said class will also be initialized with the appropriate variables if needed. Finally, if a trait extends a superclass then the companion class will not acutually inherit the superclass. Instead, any class implementing the trait will inherit it.

Pretty cool - It's always interesting to look under the hood, especially with what they're doing with this language!
