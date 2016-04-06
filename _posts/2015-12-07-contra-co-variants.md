---
layout: post
title: Contra- and Co- variance
---

{{ page.title }}
================

A small little note to remember about contra- and co- variants for the future, as
well as a mnemonic to remember which is which.

The jist of it is that a contravariant can be thought of a wildcard of a class and
all of it's inherited types, while a covariant is a wildcard of a class and all of
its subtypes. This leads to a couple of conclusions; the first being that, in a
language like java, the contravariant has a terminal type - `Object` - while the
covariant has a non-terminal type. In this way this makes it impossible for the type
checker to reason about the type of a covariant -- so it would be impossible to
assign the type and all we can hope to do is read its value. Alternatively, with a
contravariant we can reason about everything above the given type -- making it
assignable, however we could only use the greatest common type, `Object` in the case
of java, in in assignment; making it virtually useless.

Also, keep in mind that this makes abstract or inherited types _more_ powerful than
the contra- or covariants as we have access to more information. Especially in java
we will wind up using contra-/co-variant with the intent to describe items in a given
collection but, in truth, the type is describing the collection itself and not any of
its contents. Thus it is more powerful to use a more concrete type so that we can
reason about the individual items as well as describe the collection we are
interested in.

Finally, a quick little mnemonic device! As we were getting educated on
contra-/co-variants, it struck me that usually developers are more interested in
subtypes, or covariants. You can find them in iterables, for instance. Contravariants
are "contradictory to what you would expect" -- all of the inherited types.

---

On top of all of this, it looks like the concept of contra-/co- variance _does_
have some relevance in haskell as these concepts can be seen in [vectors][vec] (via
multilinear algebra and tensor analysis) and [functors][fun]! I'll have to investgate
this more later -- but to keep your interest for a brief moment more: covariant
functors (or cofunctors) are ordinary functors, while functors which invert the
morphism are the contravariants. If you want to play around with a fun library, it
looks like [Profunctors][pro] are fun to use and return a bifunctor of the
contravariant and the covariant!

[fun]:https://en.wikipedia.org/wiki/Functor
[vec]:https://en.wikipedia.org/wiki/Covariance_and_contravariance_of_vectors
[pro]:https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor.html



