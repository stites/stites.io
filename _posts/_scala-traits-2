Trait Inheritance
======================

You can add multiple traits that invoke eachother, starting with the last one.

    trait WarnLogger extends Logger {
      override def log(msg: String) {
        super.log(s"[WARN] $msg")
      }
    }

    trait ShortLogger extends Logger {
      val maxLength = 10
      override def log(msg: String) {
        super.log(
          if (msg.length <= maxLength) msg else msg.substring(0, maxLength - 3) + "..."
        )
      }
    }

Both modify message being passed to Logger.

`super.log` for traits is not the same as `super.log` for classes.

Instead, `super.log` calls the next trait in the trait hierarchy, which is dependent on the order with which traits are added. FILO order, unless something hairy comes up. So inheriting from both traits in different order results in different logs:

    val foo1 = new Foo with ConsoleLogger with WarnLogger with ShortLogger
    foo1.log('Testing logger')
    // [WARN] Testing lo...
    val foo2 = new Foo with ConsoleLogger with ShortLogger with WarnLogger
    foo2.log('Testing logger')
    // [WARN] Tes...

So beware the fact that you can't tell from a trait's definition which method will be invoked by a call to a super method. However, if you find that you do need to control which super method is called, you can specify it in brackets. With the `WarnLogger`, it would look like:

    trait WarnLogger extends Logger {
      override def log(msg: String) {
        super[ConsoleLogger].log(s"[WARN] $msg")
      }
    }

This only works with traits/classes that are __immediate__ supertypes. Anything further away in the inheritance tree won't work.

