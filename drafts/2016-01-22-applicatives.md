---
date: 1900-01-01
---



  instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty, mempty)
    (a, b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')

  instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u,f)<*>(v,x)= (u `mappend` v, f x)

Amazing! so, since Applicative has the same typeclass constraints as Functor, we find
that we can use <*> on any `snd` but not any `fst`. For that, we can rely on `Monoid`
to mappend things together.

A cool trick I just did:

    y = lookup 3 $ zip [1,2,3] [4,5,6] :: Maybe Integer
    z = lookup 2 $ zip [1,2,3] [4,5,6] :: Maybe Integer
    tupled = pure (,) <*> y <*> z      :: Maybe (Integer, Integer)

also look at:

    Prelude> const <$> [1, 2, 3] <*> [9, 9, 9]
    [1,1,1,2,2,2,3,3,3]

    Prelude> const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
    Identity [1,2,3]

Given:

    newtype Identity a = Identity a deriving (Eq, Ord, Show)
    instance Functor Identity where
        fmap f (Identity i) = Identity i
    instance Applicative Identity where
        pure i = Identity i
        (<*>) _ (Identity i) = Identity i
        (<*>) (Identity i) _ = Identity i

More code that is pretty exciting:

validateLength :: Int String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 25 a

data Person = Person Name Address deriving (Eq, Show)
mkPerson n a = case mkName n of
  Nothing -> Nothing
  Just n' -> case mkAddress a of
    Nothing -> Nothing
    Just a' -> Just $ Person n' a'

mkPerson' n a = pure Person <*> mkName n <*> mkAddress a

