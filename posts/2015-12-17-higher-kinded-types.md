---
layout: post
title: Higher kinded types
---

"higher-kinded types" exist because we have type constructors in haskell.

So, putting my jvm hat back on, imagine interface constructors which take interfaces
and types as arguments.

This requires us to expand our language:
 + type constuctors: `*->*`
 + type constants (types with _no_ constructor): `*`

> kind `*` is the kind of all standard _lifted_ types. Primatives have type `#` and
> are unlifted. lifted types are repersented by a pointer (think C) and include most
> datatypes you'll ever use. Unlifted types are machine types, represented by a value
> on the stack rather than a pointer. _they cannot be inhabited by bottom_.

So "Lifted vs Unlifted" is the same as "Boxed vs Unboxed."

---

`newtype`s are special in that they are a kind (`*`), but are unlifted because they
already represent something that is lifted. So you can only "lift" once -- it
would be convoluted to have a reference to a reference to a value.

Note that you can't compose higher-kinded types. Also keep in mind that `[]` has kind
`* -> *`. This is because the compiler knows the difference between a list of ints
and a list of strings. You can do `[] Int` or `[Int]` to get that concrete type which
would meet your expectations. To this point, you cann't expect to switch out the a
concrete type later on in your code. GHC remembers all.


