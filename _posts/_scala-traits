Overriding Traits During Instantiation
======================================

You can create a base trait with concrete methods which will be used in a class only to override that class with a different trait on impliementation.

For example:

    trait Logger {
      def log(message: String) { }  // a concrete implimentation
    }

Used in a class definition:

    class CustomerAnalytics extends Analytics with Logger {
      def foo(input: Double) {
        if (input > max) log("Insufficient funds")
        // ...
      }
      // ...
    }

Now inherit from that trait:

    trait ConsoleLog extends Logger {
      override def log(msg: String) { println(msg) }
    }

    trait LogglyLog extends Logger {
      override def log(msg: String) {
        //... use the loggly service
      }
    }

And on instantiation, use whichever trait fits the situation best:

    val analysis0 = new CustomerAnalytics with ConsoleLog
    val analysis1 = new CustomerAnalytics with LogglyLog


