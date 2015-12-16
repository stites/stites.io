With the following setup (taken from the Haskell Book):

    type Name = String
    type Age = Integer
    type ValidatePerson a = Either [PersonInvalid] a

    data Person = Person Name Age deriving Show
    data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = case age > 0 of
                  True -> Right age
                  False -> Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = case name /= "" of
                    True -> Right name
                    False -> Left [NameEmpty]

This:

    mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
    mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
    mkPerson' (Left badName) _             = Left badName
    mkPerson' _              (Left badAge) = Left badAge

is the same as:

    mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
    mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)

