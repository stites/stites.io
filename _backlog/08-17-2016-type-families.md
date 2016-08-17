Type families
==================

Just like how type classes define overloadable functions, type families define
overloadable data. Type families are useful for creating interfaces with
enhanced static information, much like dependent types.

There are two types of type families:

+ Data families are the indexed form of data and newtype definitions.

+ Type synonym families are the indexed form of type synonyms

From earlier today, we have some comparisons to draw regarding how type
families relate to multiparameter type classes / functional dependencies, and
GADTs:

+ Functional dependencies are similar since types will be conferred with
  constraints and relations. Many of these kinds of type classes can be expressed
  with type families.

+ GADTs are similar to type families since they allow a type constructor to
  depend on the type's parameters), but GADT constructors are all defined in one
  place, whereas type families can be extended.

In sort, type families are haskell's way of supporting ad-hoc overloading of
data types.

Examples
----------------------

For the time being, I'll only show the example of a data family as opposed to
the type synonym family. From [wiki.haskell][wiki-hs]:

    class GMayKey k where
      data GMap k :: * -> *
      empty       :: GMap k v
      lookup      :: k -> GMap k v -> Maybe v
      insert      :: k -> v -> GMap k v -> GMap k v

`data GMap k :: * -> *` is very interesting. it implies that `GMap` has kind `*
-> * -> *`. For the type synonym version, we would replace this line with `type
GMap k`. We could also use the `type family G a where` syntax. These alternate
version declare type synonym families, and associated type synonym families.
For more on this see "[Detailed definition of type synonym families][wiki-hs]."


An instance of this would be:

    instance GMapKey Int where
      data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
      empty                  = GMapInt  Data.IntMap.empty
      lookup k   (GMapInt m) =          Data.IntMap.lookup k m
      insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)

Notice how we define the type right at `data GMap Int v`.

[wiki-hs]: https://wiki.haskell.org/GHC/Type_families
[spj]: https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7

