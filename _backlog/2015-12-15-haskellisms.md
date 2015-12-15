When building out error types, we _must_ derive Eq or the compiler can't pattern-match on them.
Pattern matching is a `case` expression where the _constructor_ is the condition
  - there must be a way to compare constructors, so they must derive Eq
  - you can build a custom Eq, but why would you want to?

