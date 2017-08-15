---
date: 1900-01-01
---


---
layout: post
title: Lens, pragmatically
---

{{ page.title }}
================

This is just a short list of lens functions I find useful. There are a lot of
tutorials out there but, from a beginners perspective, it can be useful to just
have a "feild notes" guide to get started. Here's a build up of that guide for
myself:

-----------------

#### Lens: The quick-start guide.

**Intended for:** people who know what lenses do, but are just starting to use
them in practice

**Nice definition:** a lens is a first-class reference to a subpart of some data
type.

Important detail: a lens is _not_ a composition of getter and setter functions.

```
Lens  s t a b: s contains a, t contains b - use for Record Types
Prism s t a b: a Lens that works on Sum Types
  -> Review allows you to _build_ structure from _just a Prism_

Getter -> Is a lens which works with a Const monoid

view <lens>          parent -> returns the thing you want to see
over <lens> function parent -> modifies with function and returns the parent
set  <lens> value    parent -> overwrites with value and returns the parent

to   <function>              -> is a getter
lens <getter fn> <setter fn> -> modifies with function and returns the parent

traverse, traverse is a Traversal

ix -> a way to "index" a structure. Will return a lens which you can use for
      setting or getting

  use with:
    ^?  -> return the first (WILL USE A `First` AS THE CONTAINING MONOID)
    ^.. -> returns all of the values at some index
    has -> returns if the index exists or not

outside (EDGE OF COMFORT ZONE) -> a way to either monkeypatch or scope a pattern


look at this diagram: https://hackage.haskell.org/package/lens
smash head into keyboard 600 times
do this for 6 years
nirvana
```

I should really turn this into a literate haskell file.


References:

 - [\[1\]][schoolofhs]: School of Haskell - Little Lens Starter Tutorial

[schoolofhs]: https://perma.cc/2CZ4-SWWF

