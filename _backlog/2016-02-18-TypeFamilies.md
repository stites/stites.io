with GADTs on:

   data ThisIsAGADT a where
     ThisIsATypeFamily :: String -> ThisIsAGADT String
     --^ notice that we have bound `a` to `String`

     ThisIsAnotherTypeFamily :: ThisIsAGADT ()

