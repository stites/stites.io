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

isolated evalasync and apply

  reason the tests fail is that we’re relying on the $$root attribute to point to the root of the hierarchy. Non-isolated scopes have that attribute inherited from the actual root.

need do ensure that $$root, $$asyncQueue and $$postDigestQueue are shadowed

DESTROYING SCOPES
  In the lifetime of a typical Angular application, page elements come and go as the user is presented with different views and data. This also means that the scope hierarchy grows and shrinks during the lifetime of the application, with controller and directive scopes being added and removed.
  In our implementation we can create child scopes, but we don’t have a mechanism for removing them yet. An ever-growing scope hierarchy is not very convenient when it comes to performance – not least because of all the watches that come with it! So we obviously need a way to destroy scopes.
  Destroying a scope basically just means removing it from the $$children collection of its parent. It will thus no longer be digested recursively, making its watches effectively dead. And since the scope is no longer referenced, it will at some point just cease to exist as the garbage collector of the JavaScript environment reclaims it. (This of course only works as long as you don’t have external references to the scope or its watches from within application code.)
  Since destroying a scope just means removing it from its parent, destroying a root scope is a no-op – there’s no parent to remove it from.
  The destroy operation is implemented in a scope function called $destroy. When called, it destroys the scope.

NOTES FROM DESTROY
  $parent is prefixed with just one dollar sign instead of two. That means it’s deemed by the makers of Angular to be something that’s OK to reference from application code. However, using it is usually considered an anti-pattern because of the tight coupling between scopes that it introduces.


summary
  You have learned about:
  • How child scopes are created.
  • The relationship between scope inheritance and JavaScript’s native prototypal inherited
    Attribute shadowing and its implications.
  • Recursive digestion from a parent scope to its child scopes.
  • The difference between $digest and $apply when it comes to the starting point of
  digestion.
  • Isolated scopes and how they differ from normal child scopes.
  • How child scopes are destroyed.
  In the next chapter we’ll cover one more thing related to watchers: Angular’s built-in $watchCollection mechanism for e
