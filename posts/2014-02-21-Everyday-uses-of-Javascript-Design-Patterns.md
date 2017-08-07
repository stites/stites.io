---
layout: post
title: Everyday Uses of Javascript Design Patterns
location: San Francisco, CA
---

I've been reading _[Essential Javascript Design Patterns][designs]_ by Addy Osmani, when I realized that most design patterns are already common in everyday use of code. Inspired by this thought, I reread through my notes and decided to write out where we would see or hear about them on a daily basis.

#### Module Pattern

+IIFies, as well as RequireJS are also examples of the [module pattern][module pattern].

A variation of the module patterns, the [Module mixin][Module mixin] is used as the interface to such libraries as `jQuery`, `Backbone`, and... well basically every library out there - so long as they introduce nothing into the global scope.

If you like to introduce global variables in your Node.js files because you know they will be kept private, you are taking advantago of the [Revealing Module Pattern][Revealing Module Pattern].

#### Singleton Pattern

Angular is based off of the [Singleton Pattern][Singleton Pattern] - where only one instance is instantiated during it's lifetime. All other attempts at instantiating another instance of that singleton will create a reference to that singleton. Also, of note, it is interesting to learn that a poor implimentation of a singleton creates a public object. Just like how a variable or a function are not "closures", a properly created singleton does not create a _class_ or an _object_. It, instead, creates a _structure_: a shared resource namespace which isolates the code from the global scope. This namespace is also, accoring to the [gang of four] and [addy osmani], "extensible by subclassing. Clients should also be able to use an extended instance without modifying their code."

#### Observer Pattern

If you were to create an event listener from scratch, you might start off with two objects one that listens to the other by referencing a properties or function. This is called the [observer pattern][observer pattern] and the two objects in question are referred to by the
`subject`:
  Maintains a list of observers, facilitates adding or removing objecters
`observer`:
  probides an update interface for objects that need to be notified of a Subject's changes of state
`ConcreteSubject`:
  Broadcasts notifications to Observers on changes of state and stores the state of the `ConcreteObservers`.
`ConcreteObservers`:
  Stores a reference to the `ConcreteSubject`, implements an update interface fro the Observer to ensure taht state is consistent with the Subject's.

But if you are looking for something a little more loosely coupled, you might want to use a [Publisher/Subscriber Pattern][Publisher/Subscriber Pattern]. This is, in essence, is the pattern that would be created if we added an event listener object to mediate between two objects.

#### Mediator Pattern

If we wanted to mediate between _many_ objects, for example handling changes to DOM elements and corresponding functions, we would want to change this to a [Mediator Pattern][Mediator Pattern]. This is the event listener you are probably most familiar with. An advanced implimentation could be found at [Mediator.js] and includes "topic namespaces, subscriber removal, and a much more robust system of mediation."
--> downside: performance hit since there is not direct communication between two objects.

#### Prototype Pattern

The [prototype pattern][prototype pattern] is basically what javascript is built off of. The idea is that you create objects by cloning some object you already have. In non-javascript languages this can start to refer back to the concept of classes and inheritance, but in javascript it's plain-and-simple prototype inheritance. It's important to know that there isn't a "definition"/"core" object, just clones. This is awesome because any prototypally-inherited functions are passed by reference, so it's wicked fast. ballersauce. Addy also mentions that _real_ prototypal inheritance requires the use of `Object.create`. eg: `Object.create( prototype, optionalDescriptionObjects )`. this can get hairy when looking into what properties an object ACTUALLY has. refer to `hasOwnProperty()`.

#### Facade Pattern

jQuery uses the facade pattern all the time. When you invoke a `$(el).text()` there are actually several things you can actually do, you can return the text of some element, or you can overwrite the current text of the element. Essentially, you are masking the inner compexity of the method with a much simpler interface. This is the facade pattern. Disadvantages: performance - accessing an inner function is much more efficient than accessing the abstraction.

#### Factory Pattern

Factories are a pattern whose premise is to create objects - not classes. This distinction is what differentiates a Factory from a constructor, although this line is blurred by the fact that Javascript's pseudo-classical constructors also create objects. The factory pattern is explicitly present in Angular. In short this is because angular takes advantage of the singleton pattern which requires the creation of an object.

This pattern is useful for the construction of very complex objects - however you should be wary of using them as this can be a double-edged sword and you may find yourself dealing with _too_ much complexity.

###### The Abstract Factory

An abstract factory is a pattern which allows you to group together many factories which all have a common goal. When you create a custom factory in angular, you are taking advantage of the abstract factory pattern.

#### The Mixin Pattern

Mixins are a common pattern in Javascript which allow you to avoid subclassing. Instead of creating a superclass - `Cow` - and several subclasses - `SpottedCow`, `PurpleCow`, and `SuperCow` - you can create a mixin. This will allow you to create only one class, `Cow`, with many mixins - `addSpots`, `addPurple`, `addCape` - which can be used when appropriate.

I'm assuming that this pattern is pretty generic and am a little too lazy to find any one "big" example.

#### The Decaorator Pattern

Essentially the same as a mixin pattern, however this is used for subclassing to promote code reuse. In Javascript, of course, it is easier for us to use Pseudoclassical Decorators.

#### Flyweight Patter

I need to learn a lot more about the Flyweight pattern. I believe that it is present in D3, but this has yet to be confirmed. A lot of my shakeyness on the subject stems from the fact that Flyweights require Factories to create the data-object Here's my interpretation:

###### Data Flyweights
+ __Intrinsic__: Essentially D3. Every DOM listens to a corresponding data element. This only works if D3, indeed, uses a factory to create it's data-objects, which I believe it does.
+ __Extrinsic__: Whenever you have data-objects in D3 which have more than one DOM element attached to it. However, I'm not sure if this is possible! Another option is Backbone where you have models (data objects) which inform multiple views - again I'm uncertain on this.

###### DOM Flyweights
this describes types of event listening

+ __top-down event detection__: Event capture.
+ __bottom-up event detection__: Essentially event bubbling.

#### Patterns to be looked into

+ My favorite SQL ORM, [Bookshelf.js][bookshelf], is based off of the [data mapper pattern][data mapper pattern].
+ The command pattern doesn't seem to be very common in javascript. Usually you can see where command patterns would be used by a `.run()` or a `.exec()`.

[bookshelf]: http://bookshelfjs.org/
[data mapper pattern]: http://en.wikipedia.org/wiki/Data_mapper_pattern
[designs]: http://addyosmani.com/resources/essentialjsdesignpatterns/book/
