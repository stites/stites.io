---
layout: post
title: Coerce for constant time newtype lists
---

haskell has a coerce function. It's pretty cool!

in the haskell wiki, it talks about how `newtype`s incur no runtime cost, but
when you have an array of newtypes, a runtime cost will still happen in order to
instantiate the array of newtypes. Basically, a linear operation of noops will
occur. This can be avoided by using `coerce`.


```
import Data.Monoid
import Data.Coerce

-- Without coerce, a linear cost:
x, y, z :: Sum Int
x = [Sum 1, Sum 7, Sum 10000]

-- which is basically the same as:
y = map Sum [1,7,10000 :: Int]

-- With coerce, it becomes an O(1) operation:
z = coerce [1,7,10000 :: Int]
```



