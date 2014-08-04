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

$apply triggers a $digest from the rootscope.

  "The fact that $apply digests all the way from the root is one of the reasons it is the preferred method for integrating external code to Angular in favor of plain $digest: If you don’t know exactly what scopes are relevant to the change you’re making, it’s a safe bet to just involve all of them.
  It is notable that since most Angular applications have just one root scope, $apply does cause every watch on every scope in the whole application to be executed. Armed with the knowledge about this difference between $digest and $apply, you may sometimes call $digest instead of $apply when you need that extra bit of performance"

$evalAsync schedules a digest on the root scope, too

ISOLATED SCOPES

  At times it would be convenient to have a scope be a part of the scope hierarchy, but not give it access to everything its parents contain. This is what isolated scopes are for.

isolation is simple: make a scope part of the scope hierarchy, but do not make it inherit from its parent.

  created by passing a boolean value to the $new function. When true, isolated. When false/omitted-undefined/null prototypal inheritance will be used.

  When a scope is isolated, it doesn’t have access to the attributes of its parent

  If you’ve used isolated scopes with Angular directives, you’ll know that an isolated scope is usually not completely cut off from its parent. However this mechanism is not built into scopes. It is part of the implementation of directives. We will return to this discussion when we implement isolation in directives.

