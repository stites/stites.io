module Sum where

data Sum a b = First a | Second b deriving (Show, Eq)

-- OH! This is why Left is for Errors and Right is for values.
-- The typeclasses can operate on the right while ignoring the left
instance Functor (Sum a) where
  fmap f (First e) = First e
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure v = Second v
  (First e) <*> _ = First e
  _ <*> (First e) = First e
  (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (First e) >>= _ = First e
  (Second b) >>= f = f b

