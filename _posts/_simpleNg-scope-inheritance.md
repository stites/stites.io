notes:
scope inheritance is the exact same as javascipt's prototype chaining.
attribute shadowing - is a direct consequence of javascript's prototype chains. mainly, that inheritence goes DOWN the chain. so a parent can't read properties of it's childen, but children can see properties of its parents.

Shadowing is when you name a child var the same as a parent's var. The child has a "shadowed" var. Two instances exist on the chain and the parent's doesn't interfere with the child's

to not shadow. you can do the following:

    parent.user = {name: 'Joe'};
    child.user.name = 'Jill';

    expect(child.user.name).toBe('Jill');
    expect(parent.user.name).toBe('Jill');

this way we aren't writing anything on the child scope, we are _reading_ the parent scope's `user` then accessing it's `name`. This is part of the _Dot Rule_ see [this video](https://www.youtube.com/watch?feature=player_detailpage&v=ZhfUv0spHCY#t=1758s).

also `$$watchers` is inherited. all access to this is on the root scope.

Thus we also execute all the watches based on _scope heirarchy_. what we want is to call `$digest` on a node, then to `$digest` the node and it's children. Not siblings or parents. This requires shadowing the `$$watchers` array, so that they do not interfere with eachother, and recursively checking children's watches
