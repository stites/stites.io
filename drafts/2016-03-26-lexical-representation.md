---
date: 1900-01-01
---


one of the trickier parts of manad transformers is that the lexical representation
for the types will violate your intuitions with respect to the relationship it has
with the structure of your values.

Let us note something in the definitions of the following monad transformers (I think
you can ignore the kinds):

    newtype ExceptT e (m :: * -> *) a = ExceptT (m (Either e a))
    newtype MaybeT    (m :: * -> *) a = MaybeT  {runMaybeT  :: m (Maybe a)}
    newtype ReaderT r (m :: * -> *) a = ReaderT {runReaderT :: r -> m a   }

One requirement for MTs is that we have to wrap the monad `m` _around_ our value
It's only wrapped around things we can have, not things that we need (such as ReaderT)


