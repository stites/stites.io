source: [Why is Haskell (GHC) so darn fast?][1]

Highlights:

Haskell has full type erasure [like C, C++, Pascal…]. All type checking happens at compile-time only. So there's no run-time type-checking to slow you down either. (No null-pointer checks, for that matter. In, say, Java, the JVM must check for null pointers and throw an exception if you deference one. Haskell doesn't have to bother with that check.)

Yes, it's got an amazing type system. But you know what? That all happens at compile-time. By run-time, it's gone. Yes, it allows you to construct complicated ADTs with a line of code. But you know what? An ADT is just a plain ordinary C union of structs. Nothing more.

The real killer is lazy evaluation. When you get the strictness / laziness of your code right, you can write stupidly fast code that is still elegant and beautiful. But if you get this stuff wrong, your program goes thousands of times slower, and it's really non-obvious why this is happening.

What makes Haskell so fast? Purity. Static types. Laziness. But above all, being sufficiently high-level that the compiler can radically change the implementation without breaking your code's expectations.

Basics of how GHC works today: [Implementing Lazy Functional Languages on Stock Hardware: The Spineless Tagless G-Machine][0].

[0]: https://www.researchgate.net/publication/220676558_Implementing_Lazy_Functional_Languages_on_Stock_Hardware_The_Spineless_Tagless_G-Machine
[1]: https://stackoverflow.com/questions/35027952/why-is-haskell-ghc-so-darn-fast

