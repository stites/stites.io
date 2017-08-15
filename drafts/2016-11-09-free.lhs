---
date: 1900-01-01
---



data Free f a
  = Leaf a
  | Branch (f (Free f a))



data One a = One

Free One Int:
  - Leaf 1                                   -- same as Just 1
  - Branch (One :: One :: Free One Int)      -- same as Nothing

data I a = I a

Free I Int:
  - Leaf 1
  - Branch (I (Branch I (Leaf 1)))
  - Branch (I (Branch I (Branch I (Leaf 1))))

[1]
[_] -> [1]
[_] -> [_] -> [1]


"left-padded singlton list"

data Pair x = Pair x x

Free Pair Int:
  - Leaf 5
  - Branch (Pair (Leaf 1)  (Leaf 1) )
  - Branch (Pair (
                  Branch (Pair (Leaf 1)  (Leaf 1) )
                  Branch (Pair (Leaf 1)  (Leaf 1) )))

... a binary tree


data Pair x = Pair x x Char

Free Pair Int:
  - Leaf 5
  - Branch (Pair (Leaf 1)  (Leaf 1) 'a' )
  - Branch (Pair (
                  Branch (Pair (Leaf 1)  (Leaf 1) 'b' )
                  Branch (Pair (Leaf 1)  (Leaf 1) 'c' )) 'a' )

... a binary tree with chars in the branches
bind only cares about the leaf-nodes

what about sum types?

data T x = Pair x x Char | Q x x x | R Int

Free Pair Int:
  - Leaf 5
  - Branch (Pair (Leaf 1)  (Leaf 1) 'a' )
  - Branch (Pair (
                  Branch (Pair (Leaf 1)  (Leaf 1) 'b' )
                  Branch (Pair (Leaf 1)  (Leaf 1) 'c' )) 'a' )

... a heterogeneous tree with changing shape.

what about arrow?

data T x = P (Int -> x)

(Int -> x) is a functor: fmap :: (x-> y) -> (Int-> x) -> (Int -> y)

Free T Int:
  - Leaf 5
  - Branch (T (\n -> (Leaf 1)))
    + or Branch (T Leaf)

you get a tree shape which depends on the runtime value

-----------
revisited:

do notation:
main = (\x -> do
  y <- x          -- x is a "node" in our tree
  _ <- f y        -- the continuation of f y will build out one branch of the tree
  _ <- g y        -- the continuation of g y will build out one branch of the tree

------------
data T x = P x x Char | Q x Int

g::a -> Free f a
liftF :: f a -> Free f a
liftF g = Branch (fmap Leaf g)
liftF g = Branch (fmap pure g)

liftF is the same as lifting some `P 5 6 'a'` into a
`Branch (Leaf 5) (Leaf 6) 'a'`


p :: Char -> Free T ()
p c = liftF (P () () c)

> Branch (Leaf ()) (Leaf ()) 'a'

what does bind do?
bind creates a Branch which then

------------

data ConsoleF x
  = Print String x
  | Read (String -> x)

output :: String -> Free ConsoleF ()
output s = Branch $ Output s (Leaf ()) -- or output s = liftF (Output s ())

for some
[_] Output "foo"
 |
[()]

lets use do-notation to write a function:

main = do
  liftF (Output "foo")
  liftF (Output "bar")
  liftF (Output "baz")
  pure ()

yields:
[_] Output "foo"
 |
[_] Output "bar"
 |
[_] Output "baz"
 |
[()]


free is boilerplate-y enough that it turns out all we need to do is handle each
ConsoleF case

> interpIO :: ConsoleF x -> IO x
> interpIO = \case
>   Output s k -> do
>     putStr s
>     pure k


normally, a polymorphic function, 
polymorphic in f m a
function that's passed in must be polymorphic. so you can't pass in some @f Int -> m Int@
a higher-ranked type is just a polymorphic type AS AN ARGUMENT.

-- natural transformation
> foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a -- a rank-2 type
> foldFree phi = \case
>   Leaf a -> pure a
>   Branch g -> do
>     phi g :: m (Free f a) >>= (\k -> foldFree phi k)
>   --phi g :: m (Free f a) >>= foldFree phi

foldFree interpIO (Output "2")

> badInterpIO :: ConsoleF (Free (ConsoleF a)) -> IO (Free (ConsoleF a))
> badInterpIO _ = pure undefined {-ignore everything and insert my own code-}


> iterM :: (forall x. f (m x) -> m x) -> Free f a -> m a

> interpIO :: ConsoleF (IO x) -> IO x
> interpIO = \case
>   Output s k' -> do
>     putStr s
>     k <- k'
>     pure k


> iterM :: (forall x. f (m x) -> m x) -> Free f a -> m a
> iterM phi = \case
>   Leaf a   -> pure a
>   Branch g -> do --     g :: f (Free f a)
>     --       iterM phi    :: Free f a -> m a
>     -- fmap (iterM phi) g :: f (m a)
>     -- phi $ fmap (iterM phi) g :: m a
>     putStr s
>     k <- k'
>     pure k

data ConsoleF x
 = Output String
 | Input (String -> x)

> interpIO :: ConsoleF (IO x) -> IO x
> interpIO = \case
>   Output s k' -> do
>     putStr s
>     k <- k'
>     pure k
>   Input k -> do
>     s <- getLine
>     pure (k s)
>





------------


Pros:
flexible
allows you to specify subsets of side effects (ie: i don't want to use all of
IO, I just want to be able to use a subset of the side effects of "putline")

Cons:
everytime you do another bind, we re-traverse the tree to do it
efficiency problems (>>= is quadratic, left biased, traversing everything unless you use a codensity trick)
reflection without remorse

---------------------

Data.Functor.Sum 

type :+ = Sum
data Sum f g x
= InL (f x)
| InR (g x)

data Dice x = Roll (Int -> x)
  deriving Functor

type (:+) = Sum
infixr 5 (:+)

-- TypeOperators

output :: String -> Free (Console :+ Dice) ()


























