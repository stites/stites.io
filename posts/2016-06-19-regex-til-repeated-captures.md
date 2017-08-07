---
layout: post
title: Regex TIL- Repeating capture groups
---

{{ page.title }}
================
I consider myself decent with regexes; this comes from my data science days,
when I was doing a lot of data munging at TurboSquid. I've always considered
repeatition in capture groups "hard-mode" and usually skipped over them, as they
didn't have too much utility in my day-to-day. Today someone asked me to write
one, and here's the breakdown of how to think about it.

[Regular-expression.info][info] has a page on "Repeating a Capturing Group" vs.
"Capturing a Repeated Group", however they actually fail to explain the former.

### Capturing a Repeated Group

**Problem: getting 123abcabc123 from !123abcabc123!**

In this case, we want to repeat a `(abc|123)` and need only one capture group.
However we'll need a second group to perform the repetition. We can add a
memoryless capture `(?: )` to do this.

**Solution: `/!((?:abc|123)+)!/`**

### Repeating a Capturing Group

**Problem: get "foo-" and "bar-" from !foo- baz bar-!**

Here we basically just want to capture a `\w+-` and take advantage of the global
flap, `/g`, which I see overused and without utility. Because regex is a greedy
algorithm it will continuously discard the capture results, meaning that we have
to repeat the regex. This is where the global flag comes in. It basically says,
"once you are finished with this capture, continue performing this lookup until
you have exhausted the full string."

**Solution: `/(\w+-)/g`**

Edit: it looks like the `/g` flag will discard the input from results. I can
imagine that this makes it a little easier to work during development and is why
I see it in a lot of code where it seems unnessecary.

---

And like that - I the hardest thing I know of with regexes is now known! Very
exciting!


[info]: http://www.regular-expressions.info/captureall.html


