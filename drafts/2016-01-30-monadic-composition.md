we don't need to think about functional composition with Functor and Applicative
because it falls naturally into place.

    -- because: fmap f . fmap g == fmap (f.g)
    ghci> let interrobang'd = fmap (++ "!") . fmap (++ "?")
    ghci> let interrobang'n = fmap ((++ "!") (++ "?"))

    ghci> interrobang'd ["really...", "now..", "maybe..."]
    ["really...?!","now..?!","maybe...?!"]

    ghci> interrobang'n ["really...", "now..", "maybe..."]
    ["really...?!","now..?!","maybe...?!"]

This doesn't work for monads! We'd want something like `mcomp f g a = f (g a)`. In
ghci:

    ghci> let mcomp f g a = f (g a) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
    Couldn't match expected type ‘t2 -> (b1 -> m1 c1) -> (a1 -> m1 b1) -> a1 -> m1 c1’
                with actual type ‘t’
      because type variables ‘b1’, ‘m1’, ‘c1’, ‘a1’
      would escape their scope
    These (rigid, skolem) type variables are bound by
      an expression type signature:
        Monad m1 => (b1 -> m1 c1) -> (a1 -> m1 b1) -> a1 -> m1 c1
      at <interactive>:44:19-76
    Relevant bindings include
      g :: t1 -> t2 (bound at <interactive>:44:13)
      f :: t (bound at <interactive>:44:11)
      mcomp :: t
               -> (t1 -> t2) -> t1 -> (b -> m c) -> (a -> m b) -> a -> m c
        (bound at <interactive>:44:5)
    In the expression:
        f (g a) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
    In an equation for ‘mcomp’:
        mcomp f g a
          = f (g a) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

Klesei composition holds the anwser!

    ghci> :m + Control.Monad
    ghci> :t (>=>)
    (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

This is just like `flip (.)`:

    (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
    flip (.) ::            (a ->   b) -> (b ->   c) -> a ->   c

So we now we can compose actions very simply (from Haskell book, needs revision):

    import Control.Monad ((>=>))

    sayHi :: String -> IO String
    sayHi greeting = putStrLn greeting >> getLine

    readM :: Read a => String -> IO a
    readM = return . read

    getAge :: String -> IO Int
    getAge = sayHi >=> readM

    askForAge :: IO Int
    askForAge = getAge "Hello! How old are you?"

