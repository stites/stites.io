Trait similarity to classes
=========================
Traits can have constructors! Wow, I wasn't expecting that one. Just like a class, the constructor is embedded in the body of the trait itself. Every trait has one parameterless constructor. The fact that a trait has a parameterless constructor is the only technical difference between traits and classes. So anything you can do with a class, you can do with a trait!

    trait Foo {
      def bar(x: String)
      def log(y: String) {}

      // part of the constructor
      val baz = 10
      out.println('bats')
    }

Trait constructors execute in the following order:

  + The superclass constructor is called
  + Traits are constructed left-to-right
    + In each trait, its parents are constructed first
    + If multiple traits share a common parent and the parent has already been constructed, it is not constructed again.
  + the subclass constructor is called

If we want to evaluate some inherited logic from the fields of the file logger, because of the construction order we need have one of two options: lazy evalutaion, or early definition blocks. In some class `Foo` which inherits from a `FileLogger`:

    trait FileLogger extends Logger {
      val filename: String
      lazy val out = new PrintStream(filename)
      def log(msg: String) { out.println(msg) } // No override needed
    }

Evaluating `out` lazyly will give us time to instantiate `filename` before running `out`. Alternatively we can define `filename` early in an instance:

    val foo = new { val filename = "myapp.log" } with Foo with FileLogger

or in the class definition:

    class Foo extends {
      val filename = "savings.log"
    } with Account with FileLogger {
      // Foo implementation
    }

Since traits are technically the same as a class, but with that one hitch of being parameterless, it stands that a trait can extend a class and make it a superclass of any class mixing the trait.

For example we have a `trait Foo extends ClassSuper` and a `class Sub extends Foo`. Then the superclass of `Foo`, `ClassSuper` will be passed on to become the superclass of `Sub`. This is even okay if our class extends a seperate class and only uses `Foo` as a trait so long as all classes are a subclasses of the trait's superclass. Only one superclass can exist.







