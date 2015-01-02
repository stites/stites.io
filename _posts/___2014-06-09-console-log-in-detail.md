---
layout: post
title: `console.log` in detail
---

{{ page.title }}
================

<p class="meta">09 June 2014 - Redwood City, CA</p>

String Substitutions:

|  Substitution string  |  Description  |
|-----------------------|---------------|
|  %o                   |  Outputs a hyperlink to a JavaScript object. Clicking the link opens an inspector. |
| %d or %i              |  Outputs an integer. Formatting is not yet supported. |
| %s                    | Outputs a string. |
| %f                    | Outputs a floating-point value. Formatting is not yet supported. |


`console.count()`

`console.dir()`
  Displays an interactive listing of the properties of a specified JavaScript object. This listing lets you use disclosure triangles to examine the contents of child objects.

`console.error()`
  Outputs an error message. You may use string substitutions with this console method.

`console.group()`

`console.groupCollapsed()`

`console.groupEnd()`

`console.info()`

`console.log()`
  console.debug(object[, object...]) Deprecated since Gecko 5.0
  An alias for log(); this was added to improve compatibility with existing sites already using debug(). However, you should use console.log() instead.
`console.time()`

`console.timeEnd()`

`console.trace()`

`console.warn()`


[]: 
