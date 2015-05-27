---
layout: post
title: Scala Inheritance
---

{{ page.title }}
================

<!-- <p class="meta">27 May, 2015</p> -->

`final` keywords are the same as in Java
you can override fields.

#### Classes inheriting from classes
`extend` is the same as in Java. To have `Foo` inherit from `Bar`, the syntax would be:

```
  class Foo extends Bar { ... }
```

You have to use the override modifier on anything that isn't abstract.
This is good for solving the _fragile base class problem_ when a change to the
superclass cannot be verified without looking at all subclasses.

Java initially solves this by making everything final — but it also means that making little changes to methods, like logging, requires a high impact on changed code. This is when `@Overrides` annotation was introduced.

Invoking superclasses is the same as in Java — use `super.methodName`.

#### Type checking and type casting
This can be summed up in the following table:
<table>
  <thead>
    <tr>
      <th>Scala</th>
      <th>Java</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>obj.isInstanceOf[C1]</td>
      <td>obj isInstanceOf C1 </td>
    </tr>
    <tr>
      <td>obj.asInstanceOf[C1]</td>
      <td>(C1) obj</td>
    </tr>
    <tr>
      <td>classOf[C1]</td>
      <td>obj.class</td>
    </tr>
  </tbody>
</table>

However pattern matching is the recommended alternative. Pattern matching looks more like:

> obj match {
>   case obj: Foo => ... // obj is an instance of Foo in this block
>   case _ => ... // obj wasn't an instance of Foo
> }

#### Protected Fields and Methods
In a class, you can declare fields or methods to be `protected`. Protected members are only accessible from subclasses, in the same way one would expect from Java. However, unique to Scala, a `protected` member cannot be seen in the package it belongs to without a [package modifier][packageModifier].

There is also a `protected[this]`, which restricts access to only the current object, just like `private[this]` [discussed in Chapter 5][private].

#### Superclass construction
There is only one primary constructor per class, and this is the only place you can invoke a superclass constructor directly. This is because of the daisy-chaining of auxilary constructors.

However, primary constructors are embedded in class definitions, so it would follow that the superclass call can, similarly, be embedded. This winds up looking like:

> class Foo(x:Int, y:String) extends Bar(y) {...}

Alternatively, you can make a call to `super` in the primary constructor — but you can't use the usual `super(params)` call you would expect in scala.

#### Java interop
Scala classes can extend Java classes!

#### Overriding Fields
scala fields are more than just a simple java field. It includes a private field, as well as accessor and mutator methods (getters and setters). With this in mind, it limits and informs us of what kinds of overrides we can perform:

<table>
  <thead>
    <tr>
      <th></th>
      <th>...with `val`</th>
      <th>...with `var`</th>
      <th>...with `def`</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Override `val`...</td>
      <td>Expected scala override with private field, setters and getters</td>
      <td>Error</td>
      <td>Error</td>
    </tr>
    <tr>
      <td>Override `var`...</td>
      <td>Error</td>
      <td>Only if the superclass is abstract</td>
      <td>Error</td>
    </tr>
    <tr>
      <td>Override `def`...</td>
      <td>Expected scala override with private field, setters and getters</td>
      <td>Basic Java override</td>
      <td>Like in Java</td>
    </tr>
      </tbody>
</table>

Note that if you use `var` variables in your classes, your fellow programmers will be stuck with it when subclassing!

#### Anonymous Subclassing
This is the same as in Java. If you make an instance of an anonymous subclass if you include a block with definitions or overrides. Look at:

>val anon = new Foo(42, "bar"){
>  def newMethod = "check it out!"
>}

This creates an object of _sturctural type_ - `Foo{def newMethod: String}`.

#### Abstract classes
Are just like abstact classes in Java, with the one exception that if you want to declare an abstact method or field, you simply need to omit its body or value. When subclassing, you also do not need to declare any _override_ keywords.

[You can always customize abstract fields by using an anonymous type.][]

#### Construction order

8.10 Construction Order and Early Definitions image

When you override a val in a subclass and use the value in a superclass constructor, the resulting behavior is unintuitive.

Here is an example. A creature can sense a part of its environment. For simplicity, we assume the creature lives in a one-dimensional world, and the sensory data are represented as integers. A default creature can see ten units ahead.

class Creature {
  val range: Int = 10
  val env: Array[Int] = new Array[Int](range)
}

Ants, however, are near-sighted:

class Ant extends Creature {
  override val range = 2
}

Unfortunately, we now have a problem. The range value is used in the superclass constructor, and the superclass constructor runs before the subclass constructor. Specifically, here is what happens:

1. The Ant constructor calls the Creature constructor before doing its own construction.

2. The Creature constructor sets its range field to 10.

3. The Creature constructor, in order to initialize the env array, calls the range() getter.

4. That method is overridden to yield the (as yet uninitialized) range field of the Ant class.

5. The range method returns 0. (That is the initial value of all integer fields when an object is allocated.)

6. env is set to an array of length 0.

7. The Ant constructor continues, setting its range field to 2.

Even though it appears as if range is either 10 or 2, env has been set to an array of length 0. The moral is that you should not rely on the value of a val in the body of a constructor.

In Java, you have a similar issue when you call a method in a superclass constructor. The method might be overridden in a subclass, and it might not do what you want it to do. (In fact, that is the root cause of our problem—the expression range calls the getter method.)

There are several remedies.

• Declare the val as final. This is safe but not very flexible.

• Declare the val as lazy in the superclass (see Chapter 2). This is safe but a bit inefficient.

• Use the early definition syntax in the subclass—see below.

The “early definition” syntax lets you initialize val fields of a subclass before the superclass is executed. The syntax is so ugly that only a mother could love it. You place the val fields in a block after the extends keyword, like this:

class Bug extends {
  override val range = 3
} with Creature

Note the with keyword before the superclass name. This keyword is normally used with traits—see Chapter 10.

The right-hand side of an early definition can only refer to previous early definitions, not to other fields or methods of the class.

Notes:

+ You can debug construction order problems with the -Xcheckinit compiler flag. This flag generates code that throws an exception (instead of yielding the default value) when an uninitialized field is accessed.

+ At the root of the construction order problem lies a design decision of the Java language—namely, to allow the invocation of subclass methods in a superclass constructor. In C++, an object’s virtual function table pointer is set to the table of the superclass when the superclass constructor executes. Afterwards, the pointer is set to the subclass table. Therefore, in C++, it is not possible to modify constructor behavior through overriding. The Java designers felt that this subtlety was unnecessary, and the Java virtual machine does not adjust the virtual function table during construction.

#### Scala Inheritance Heirarchy

[img](https://www.safaribooksonline.com/library/view/scala-for-the/9780132761772/graphics/horstmann_inheritance.jpg)

Classes that are primative types in Java, as well as `Unit`, extend `AnyVal`. All other classes are subclasses of the `AnyRef` class (synonymous to `Object` in Java). Both `AnyVal` and `AnyRef` extend `Any` class - the root of the heirarchy.

`AnyVal` is a marker for value types, while `AnyRef` adds the monitor methods `wait` and `notify`/`notifyAll` fro the `Object` class. Also there is a synchronized method with a function parameter which is the same as a `synchronized` block in Java. All scala classes implement the marker interface `ScalaObject` which has no methods.

In contrast to `AnyVal` and `AnyRef`, are `Nothing` and `Null`. `Null` is the type whose sole instance is the value `null`. You can assign `null` to any reference type (not value types). The `Nothing` type has no instances. It's used for generic constructs like the empty list - `Nil`, or `List[Nothing]` - whic is a subtype of a generic list - `List[T]`.

The `Nothing` type is not the same as a `void` in Java - the Java `void` is synonymous to `Unit` in Scala. `Unit`'s value is `()` and is not a supertype of any other type, but the compiler allows any value to be _replaced_ by a `()`.

#### Object Equality
use `eq` to see if two references refer to the same object. Note that the `equals` method in `AnyRef` calls `eq`.

Consider overriding the `equals` method for a natural notion of equality for your situation. for insance comparing items by metadata instead of the class type. Keep in mind that, when you do this, you want group symmetry - where `a.equals(b)` and `b.equals(a)`. Also be sure that `equals` takes type of `Any`.

#### Classes inheriting from traits




[packageModifier]: chapter 7
[private]: chapter 5