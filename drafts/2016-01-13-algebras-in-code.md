In haskell, there is often a closeness to using mathematical phrases like "algebra"
in the concrete term of code. Stephen Dheil has a great post on what an algebra is,
here's a brief snippet from myself and the FPHP book:

> An algebra consists of a set, as well as one or more operations which operate over
> this set.

In a perfect OOP world written with purity and a nice typesystem (Java, Typescript, etc),
this would look like a combination of an interface and... Actually, I believe it
would just be an interface which defines an algebra. It would then be up to the
programmer to make this algebra concrete by implimenting a type which uses this
interface.

