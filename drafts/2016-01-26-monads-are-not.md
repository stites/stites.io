---
date: 1900-01-01
---


they are not "IO" -- IO has an instance of Monad
they are not impure -- IO is an abstract datatype that allows for impure and has a
                       Monad Insatnce
They are often used for sequencing actions in a way that looms imperative.
they are not a value. the typeclass describes a specific relationship between
elements in a domain and some operations over them.
Bind and Return are non-strict.

