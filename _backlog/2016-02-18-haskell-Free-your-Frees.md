I was working on a project of Alex Crough's at Haskell Hack Night which takes the
Free monad to a whole 'nother level. The primary reasoning behind Elision being that
`Free` is actually not "free" enough when it comes to constaints and testing. An
Elision doesn't require that you pass it a functor, and it's possible to unit-test
the business logic that resides in the resulting Monad by simple "hot-swapping" which
feels something like a "mock." In fact it feels more like mocking than anything I've
seen in haskell so far.



http://crough.io/elision/
