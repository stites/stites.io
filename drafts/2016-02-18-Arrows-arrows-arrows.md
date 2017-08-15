Really cool things about Arrows:

+ Every pure function is a mapping from some `a` to some `b`. Thus every pure function
  can form an arrow from `a`s to `b`s. To promote any function to an arrow, we can use
  the `arr` function invoked on that endofunctor.

Standard arrows:

    &&& is "fanout" which takes an input, applies both arrows,
           and returns the tuple of the result for each arrow.
    *** is "both" which takes two arrows and applies each to the
           relevant sides of the tuple.
    ||| is "fanin" which takes an Either and will apply the
           relevant arrow to the appropriate side.
    +++ is the same as "fanin" but will maintain structure and will
           return an Either.
    >>> is "left-to-right composition"
    <<< is "right-to-left composition"
    >>^ is "left-to-right precomposition"
    ^>> is "left-to-right precomposition"
    <<^ is "right-to-left precomposition"
    ^<< is "right-to-left precomposition"






