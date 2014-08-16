we can watch things by ref and value (by passing the third bool parameter in in $watch)
now check out `$watchCollection`
 + it's for array/obj - add, remove, rerodered events
 + talk about (Object.watch segway)

passing in the last param in watch is overkill for this. deep watch entire object - we only want high level.

$watchCollection is just a shallow check, but learning it will teach us how to make specialized watches for custom datastructures


#### Setting up the infrastructure
$watchCollection is a higher-order function which `delegates` to $watch
takes the $watchers that you write and wraps them so that they can be added to dhe $watchers array(via $watch)

#### detecting non-collection changes
it also works for non-collectinos - it just falls back to $watch

------------

    Scope.prototype.$watchCollection = function(watchFn, listenerFn) {
      var self = this;
      var newValue;
      var oldValue;

      var internalWatchFn = function(scope) {
        newValue = watchFn(scope);
        oldValue = newValue;
      };
      var internalListenerFn = function() {
        listenerFn(newValue, oldValue, self);
      };
      return this.$watch(internalWatchFn, internalListenerFn);
    };

> by keepint newValue and oldValue declarations outride of the internal watch functino body, we can share them between the internal watch and listener functions. They will also persist between digest cytles in the closure formed by the $watch Collection function. this is particularly important for the old Value, since we need to compare to it acsorr digest cycles.

> the listener function just delegates to the original listener function, passing it the new and old values, as well as the scope

------------

digest determines whether the listener should be called or not by the return value of the listener.
watchCollection only needs to know that the old and new values are different. That's it. so angular team chose to just increment a counter when a change happens.


common snags: NaN (page 105)

### Detecting New Arrays

internal watch checks two things:
  objects
    arrays
    objects
  everythign else.

we use simple conditionals to make decision trees to find out what to
do and we really just detect new or removed items by saving the length on the oldValue.

### Detecting Replaced or Reordered Items in Arrays

aside from changes in obj size, we need to see if things are swapped out. so now we actually need to iterate.

### Array-like objects
`arguments` array! also `NodeList` which is how you get `quesySelectorAll` and `getElementsByTagName`

### Detecting new objects
objects other than arrays: mainly dicts

### Detecting New Or Replaced Attributes in Objects.
We want new attrs added to an obj to trigger a change
We want changed attrs added to an obj to trigger a change

While we iterate, we also sync the old object with the attributes of the new object, so that we have them for the next digest.

> The hasOwnProperty clause in the for loop is a common JavaScript idiom for checking that a property is attached to the object itself instead of being inherited through the prototype chain. $watchCollection does not watch inherited properties in objects.

### Detecting Removed Attributes in Objects
it('notices when an attribute is removed from an object',

arrays are easy, we can check length - another loop

### Preventing Unneseccary Object Iteration
we really don't want to add another loop. probjematic at larpe objs
optimization:
  for the oldObj, keep a car that increments whenever an attr added, decerment whenever attr removed
  for the new obj, calculate size during first loop in internal watch

