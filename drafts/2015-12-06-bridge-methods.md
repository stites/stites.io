---
date: 1900-01-01
---


Bridge Methods
--------------

    public interface Context<E> {
      E getInstance();
    }

    public interface DataContext<E extends Data> extends Context<E> {
      E getInstance();
    }

    public class TestContext implements DataContext<Test> {
      public Test getInstance() { ... }
    }

I was looking through my code looking for a good example of this, but I think
znetdevelopment.com really does have the best example of bridge methods. Here, on the
instance of a given `TestContext`, you would think that the resulting byte code has a
single method, `getInstance`, which returns a `Test`. You would actually be
incorrect. Three methods are generated, with the two shadowed methods being "bridge
methods." These shadowed methods will return `Data` and `Object` -- the boundary types
of the `DataContext` and `Context` methods, respectfully.

