Abstract Methods in Scala Traits
===========================================

When you want to override an abstract method in a traits, you must declare both `abstract` on top of our `override` in order for this to work:

    trait Logger {
      def log(msg: String)  // abstract
    }

    trait WarnLogger {
      abstract override def log(msg: String){
        super.log(s"[WARN] $msg")
      }
    }

If you fail to add the `abstract` modifier, Scala will think that the log method is concrete, will find an abstract log instead, and the compiler will error.

A trait can also have methods have abstract dependencies. One instance of this is the scala `Iterator` trait which has a ton of dependencies on the abstract `next` and `hasNext` methods.

An example with our Logger:

    trait Logger {
      def log(msg: String)
      def info(msg: String)  { log(s"[INFO] $msg")  }
      def warn(msg: String)  { log(s"[WARN] $msg")  }
      def error(msg: String) { log(s"[ERROR] $msg") }
    }

Which we can use like so:

  class Foo with Logger {
    override def log(msg:String) { println(msg); }

    def bar(msg: String) {
      error("Trouble ahead") // logs: '[ERROR] Trouble ahead'
    }
  }

