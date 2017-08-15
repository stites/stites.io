---
date: 1900-01-01
---


ghci> let print1 = putStrLn "1"
ghci> let print2 = putStrLn "2"
ghci> let printComposite = (print1, print2)
ghci> :t printComposite
printComposite :: (IO (), IO ())
ghci> fst printComposite
1
ghci> snd printComposite
2
ghci> fst printComposite
1

