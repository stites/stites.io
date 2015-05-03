Overriding abstract methods in scala traits
===========================================

When you want to override an abstract method in a
traits, you must declare both `abstract` on top of
our `override` in order for this to work:

    trait Logger {
      def log(msg: String)  // abstract
    }

    trait WarnLogger {
      abstract override def log(msg: String){
        super.log(s"[WARN] $msg")
      }
    }

If you fail to add the `abstract` modifier, Scala
will think that the log method is concrete, will
find an abstract log instead, and the compiler will
error.
