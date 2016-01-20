typeclasses have unique pairings of the class and the instance for a particular
type.

An orphan instance is a big deal. It's when you have an instance for a datatype
and a typeclass, but the instance is not in the same module as said datatype or
typeclass.

If you don't own the typeclass/datatype. you can `newtype` it! Even when you
want an orphan instance so that you can have multiple instances for the same
type, you still want to use newtype.

module Listy where

newtype Listy a =
    Listy [a]
    deriving(Eq, Show)

module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = List []
  mappend (Listy l) (Listy l') = Listy $ mappend l l'


