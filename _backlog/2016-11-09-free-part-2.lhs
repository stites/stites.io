multiparameter typeclass

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE LambdaCase #-}





> data Free f a
>   = Leaf a
>   | Branch (f (Free f a))
> 
> instance Functor f => Functor (Free f) where
>   fmap f (Leaf a) = Leaf (f a)
>   fmap f (Branch fThingy) = Branch (fmap (fmap f) fThingy)
>
>
> instance Functor f => Applicative (Free f) where
>   pure = Leaf
>   (<*>) = undefined
> 
> instance Functor f => Monad (Free f) where
>   return = Leaf
>   Leaf a >>= f = f a
>   Branch g >>= f = Branch (fmap (f =<<) g) -- a monad falls out
> 
> data Proxy a = Proxy
> 
> type ProxyF a = Free Proxy a
> 
> 
> data Console x
>   = Output String x
>   | Input (String -> x)
>   deriving (Functor)
> 
> liftF :: Functor f => f a -> Free f a
> liftF = Branch . fmap Leaf
> 
> output :: String -> Free Console ()
> output s = liftF (Output s ())
> 
> input :: Free Console String
> input = liftF (Input id)
> 
> program :: Free Console ()
> program = do
>   output "hi"
>   output "ho"
> 
> 
> another :: Free Console ()
> another = do
>   output "type something"
>   foo <- input
>   output ("you said: " ++ foo)
> 
> 
> interpIO :: Console x -> IO x
> interpIO (Output str k) = putStrLn str >> return k
> interpIO (Input k) = fmap k getLine
> 
> 
> interpIO' :: Console (IO x) -> IO x
> interpIO' (Output str k) = putStrLn str >> k
> interpIO' (Input k) = getLine >>= k
> 
> 
> iterM :: (Functor f, Monad m) => (forall x. f (m x) -> m x) -> Free f a -> m a
> iterM phi f =
>   case f of
>     Leaf a -> pure a
>     Branch g -> phi (fmap (iterM phi) g)
> 
> 
> foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
> foldFree phi f =
>   case f of
>     Leaf a -> pure a
>     Branch g -> phi g >>= foldFree phi
> 
> 
> foo :: Free Console () -> IO ()
> foo = foldFree interpIO
> 
> main :: IO ()
> main = foldFree interpIO program -- interpIO program





















> data Dice x
>   = Roll (Int -> x)
>   deriving Functor
>
> data Sum f g x
>   = InL (f x)
>   | InR (g x)
>   deriving Functor
>
> type (:+) = Sum

both f && g are * -> *

> class (Functor f, Functor g) => f :< g where
>   inj :: f a -> g a
>
> instance Functor f => f :< f where
> 
>   inj = id
>
> instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :< (f :+ g) where
>   inj f = InL f
>
> instance (f :< g, Functor h) => f :< (h :+ g) where
>   inj f = InR (inj f)

when you try to call @A -> A :+ B@, GHC will yell. This is because GHC is
looking for two dicts to resolve, @f :< (h :+ g)@ and @f :< (f :+ g)@. They
overlap. Fix this with the -XOverlappingInstances pragma. This will allow GHC to
resolve the overlapping case, and also choose not to use it when it thinks
everything is good.

Now we can write our smart constructors like:

> output' :: (Console :< f) => String -> Free f ()
> output' s = liftF $ inj (Output s ())

> roll :: Free Dice Int
> roll = liftF (Roll id)

> roll' :: (Dice :< f) => Free f Int
> roll' = liftF $ inj (Roll id)

> input' :: (Console :< f) => Free f String
> input' = liftF $ inj (Input id)

> sample :: (Console :< f, Dice :< f) => Free f ()
> sample = do
>    n <- roll'
>    output' "Guess"
>    m <- input'
>    if n == read m
>      then output'   "Yup"
>      else output' $ "It was " ++ show n

  consoleIO :: Console ~> IO

  diceIO :: Dice ~> IO

> diceIO :: Dice a -> IO a
> diceIO = \case
>   Roll k -> pure (k 6)

> (+:) :: (f a -> r) -> (g a -> r ) -> (Sum f g a -> r)
> far +: gar = \case
>   InL f -> far f
>   InR g -> gar g


foldFree :: (f ~> m) -> Free f ~> m
pattern match sum, 

consoleIO and diceIO to get foldFree pick apart relevant f for you


foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree phi f =
  case f of
    Leaf a -> pure a
    Branch g -> phi g >>= foldFree phi

















