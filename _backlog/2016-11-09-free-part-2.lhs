multiparameter typeclass

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE ScopedTypeVariables #-}





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


=================================

New Program:

> data Die x
>   = Die String
>   deriving Functor

is a functor

fmap :: (a -> b) -> Die a -> Die b

but leavse the string alone

> die :: Die :< f => String -> Free f a
> die = liftF . inj . Die

in order to add this to our Die + Console + Dice, we need to create some
interpreter.

dieIO :: Die ~> IO

but we can't write this

(~>) f g :: forall x . f x -> g x

dieIO :: Die ~> IO
dieIO (Die s) = do
  putStrLn s
  pure undefined -- cause we don't know what the type of Die is
  -- or we could exit program. not desired

semantically we want a Free f a -> IO (Either String a), but this doesn't
maintain the natural transformation ~>


the idea is to have a non-pure-ish? transformation

> runDie :: Functor f => Free (Die :+ f) a -> Free f (Either String a)

take a program with a die effect and translate it into a "die-less" free monad

we can't run the Console + Die, but that's okay because we don't pick the
ordering, the intrepreter does

The mapping to Either does not constrain the Console or Dice


> runDie = \case
>   Leaf a -> Leaf (Right a)
>   Branch (InL (Die s)) -> Leaf (Left s)
>   Branch (InR g) -> Branch (fmap runDie g)

here we are punting the computation of runDie into g, since we have no
understanding of what happens next


let's write some eliminate to abstract away the boilerplate of our runDie
effect. This will be, in essence, our effect handler

> eliminate :: (Functor f, Functor g)
>           => (a -> b)
>           -> (forall x . f x -> (x -> Free g b) -> Free g b)
>           -> Free (f :+ g) a -> Free g b
> eliminate pur imp = \case
>   Leaf a -> Leaf (pur a) -- :: Free g b
>   Branch (InL f) -> imp f (eliminate pur imp)
>   Branch (InR g) -> Branch (fmap (eliminate pur imp) g)


> runDie' :: forall g a . (Functor g) => Free (Die :+ g) a -> Free g (Either String a)
> runDie' = eliminate pur imp
>   where
>     pur :: a -> Either String a
>     pur = Right
>
>     imp :: forall x . Die x -> (x -> Free g (Either String a)) -> Free g (Either String a)
>     imp (Die s) k = Leaf (Left s)
>

> sample2 :: (Console :< f, Dice :< f, Die :< f) => Free f ()
> sample2 = do
>   output' "Guess"
>   n <- input'
>   m <- roll'
>   if (read n :: Int) == m
>     then die "Bang"
>     else do
>       output' "click"
>       sample2

foldFree (interpIO +: rollIO) (runDie' sample)

----------------------------------------------------------

> data State s x
>   = Get (s -> x)
>   | Put s x

> get :: (State s :< f) => Free f s

get = Branch  (foo :: f Free f s), we don't know what f is, so we can inject
inj :: State s x -> f x

would want to pick (Free f s)

> get = Branch ( inj ( Get (\s -> Leaf s)))

with liftF:

      = liftF (inj (Get id))


> put :: (State s :< f) => s -> Free f ()
> put s = liftF (inj (Put s ()))

State s ~> IO 

stateIO s = \case
  Get k -> pure (k s)
  Put s' k -> pure k -- s' is ignored!!!


> runState :: Functor f => s -> Free (State s :+ f) a -> Free f (s, a)
> runState s = \case
>   Leaf a -> Leaf (s, a)
>   Branch (InL (Get k)) -> runState s (k s)
>   Branch (InL (Put s' k)) -> runState s' k
>   Branch (InR g) -> Branch (fmap (runState s) g)

if you do a runState and then a runDie
Free (Die :+ State Int :+ f) a -> Free (State Int :+ f) (Either String a)
                               -> Free f (Either String a, s)

if you do a runDie and then a runState
Free (Die :+ State Int :+ f) a -> Free (State Int :+ f) (Either String a)
                               -> Free f Either String (a, s)

packages for this: extensible effects, freer


data Console x
 = Output String x
 | Input (String -> x)
 | SetEcho Bool x

setEcho False 



