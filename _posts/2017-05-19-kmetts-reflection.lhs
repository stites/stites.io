To be run in ghci with @stack ghci --package reflection@

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Reflection
import GHC.TypeLits
import Data.Proxy

main :: IO ()
main = do
  print $ reify 6 (\p -> reflect p + reflect p)
  print $ reifyNat 5 ((\p -> p + 30) . natVal)
  print $ reifyNat 5 (\p -> natVal p + 30)
  print $ reifyNat 5 (\p -> reflect p + 30)
  print $ reify True  oink
  print $ reify False oink
\end{code}

foo

\begin{code}
instance Reifies Int Bool where
  reflect _ = True

instance Reifies Char Bool where
  reflect _ = True

oink :: Reifies s Bool => Proxy s -> String
oink p = if reflect p
           then "oink"
           else "moo"

class FetchLatest a where
  fetchLatest :: IO a

instance FetchLatest UserInfo where
  fetchLatest :: IO UserInfo
  fetchLatest = -- from redis

instance FetchLatest LogEntry where
  fetchLatest :: IO LogEntry
  fetchLatest = -- from filesystem

\end{code}

class FetchLatest Bool where
  fetchLatest :: RedisConn -> IO Bool
  fetchLatest = -- from redis

infected fetch with specific concern (rectifiable, use typefamily). input to fetch function. might be what you want, but it introduces programmer error on each parameter invocation.

reflection -- used for modular arithmetic -- look for kmett talk / paper / post

