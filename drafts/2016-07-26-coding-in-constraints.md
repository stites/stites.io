---
date: 1900-01-01
---


"Haskell has an incredibly powerful type system."

Is a phrase you hear a lot. Perhaps it's because haskell wasn't my first foray
into functional and strongly typed languages, but the type system always came
across more as "super-nice" as opposed to "powerful." "super-nice" means that it
has everything I _want_ in a type system to make my code bug-free, feel like I'm
programming that keeps me sane, and read like psuedocode.

Calling a type system "powerful" requires you to learn something that changes
your everyday way of coding in a manner that is unique to the language. For that
reason, I think haskell's "power" has comes from teaching me functional
programming practices. Just yesterday, however, I learn about a greater "power";
one that isn't functional in nature, but comes solely from the type system.

I think it leads me to believe that Haskell's type system is easy to be used
wrong. If you try to build projects in haskell using only easy-to-find
resources, you will most likely rely on types in a way that might make your
brain hurt when you start to think about things like testing. The secret to this
brain teaser, however, is the idea of "deferring implementation till the last
possible moment." It's something that you hear whispers of with things like
`free`. Josh pointed me in the direction of constraints-based programming,
however, which is a simple way to lift your code to a higher abstraction
immediately and allows you to take the code you write everyday and make it
implementation-agnostic.

It, essentially, takes the idea of duck-typing and makes it pure - I'll go out
on a limb here and also say that mocking is a kind of duck typing.  You never
have to lie to the complier because you never made any promises in the first
place.


