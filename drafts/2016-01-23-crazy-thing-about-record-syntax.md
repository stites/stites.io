---
date: 1900-01-01
---


This works!!!!

    validateLength :: Int -> String -> Maybe String
    validateLength maxLen s = if (length s) > maxLen then Nothing else Just s
    newtype Name = Name String deriving (Eq, Show)
    newtype Address = Address String deriving (Eq, Show)
    mkName :: String -> Maybe Name
    mkName s = fmap Name $ validateLength 25 s
    
    mkAddress :: String -> Maybe Address
    mkAddress a = fmap Address $ validateLength 25 a
    
    
    data Person = Person { name::Name, addy::Address} deriving (Eq, Show)
    mkPerson n a = case mkName n of
      Nothing -> Nothing
      Just n' -> case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person { name = n', addy = a' }
    
    mkPerson' n a = Person <$> mkName n <*> mkAddress a
    
