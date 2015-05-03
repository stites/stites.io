Trait Fields are not Inherited
==============================

I mentioned in an earlier post that methods declared, but not defined, are implicitly defined as abstract:

    trait Foo {
      def bar(x: String)     // abstract definition
      def baz(y: String) {}  // concrete definition
    }

The same goes with fields:

    trait Foo {
      val bar       // abstract field
      val baz = 10  // concrete field
    }

When a class inherits from a trait, these fields are placed into the subclass - _not_ inherited. Note that the JVM itself only allows for one superclass, so traits can't be inherited in the same way - hence this distinction.
