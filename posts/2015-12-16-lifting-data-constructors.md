---
layout: post
title: lift your constructors for healthy data
---

{{ page.title }}
================

When constructing a new data type, it makes sense that we'd want to have some
validation to ensure that our inputs are correct. Say we have some

    type Name = String
    type Age = Integer
    data Person = Person Name Age deriving Show

Clearly, we want to make sure that someone's age isn't less than 0 -- and I'm pretty
sure we also don't want to have a [_Bobby Tables_][name] situation on our hands. It's
also a good idea not to leave the name empty. To keep things simple, we'll stick to
the latter and make sure that our humans are valid:

    type ValidPerson a = Either [PersonInvalid] a

and let's create our error type and validation checks:

    data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = case age > 0 of
                  True -> Right age
                  False -> Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = case name /= "" of
                    True -> Right name
                    False -> Left [NameEmpty]

This all seems sane, but now how are we going to make our people? Well, the first
approach would be to create some function that checks all of our conditions:

    mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
    mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
    mkPerson' (Left badName) _             = Left badName
    mkPerson' _              (Left badAge) = Left badAge

...but seeing as `Either` is an applicative monad, we can use `lift` to clean this
up a smidge:

    mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
    mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)

I should give credit where credit is due, and so I would be remise if I didn't
mention that the code examples come from the Haskell Book, which I am going through
at the time of this writing. Also, I couldn't resist the cheesy title.

[name]: https://xkcd.com/327/
[book]: haskellbook.com
