---
date: 1900-01-01
---


```
    ghci> :t fmap (<*>)
    fmap (<*>) :: (Functor f, Applicative f1) =>
        f (f1 (a -> b)) -> f (f1 a -> f1 b)
```
