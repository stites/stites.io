---
date: 1900-01-01
---


---
layout: post
title: Reflections on API architecture
---

{{ page.title }}
================

Recently I was asked a question about API architecture and wanted to share some
thoughts.

To keep in mind:
+ Good api design is a balance of client-focused functionality with developer-focused
  maintainability.
+ Documentation is right up there as one of the most important things
  - The point of documentation / specifications is that they are closer to the "user
    story." Given that APIs are user-first, this is vital.
+ Stability is the number one feature
  - versioning should be as painless as possible
+ security should be a #1 priority in all decisions
+ wrt versioning with sdk-APIs (specifically in java) you may be better off shipping
  abstract classes to your customers than interfaces (comes from Effective Java),
  but now that we have defender methods, I'm not so sure this is true.

Personal pain points:
+ Paging. Paging all the big things - however given that the uniform access principle
  is a very nice thing, perhaps everything should be paged

Things to keep in mind, but are not always apparent when making your API decisions:
+ Auditing

Open questions:
+ code reuse across versions
  - I think it should be clear that as much code should be reused as possible and
    developers shouldn't understand what a "version" is. Versions should be cut from
    a stable portion of code.

[stripe-ex]:https://stripe.com/docs/api
[github-ex]:https://developer.github.com/v3/
[dissertation]:https://www.ics.uci.edu/~fielding/pubs/dissertation/rest_arch_style.htm
[se-radio]:http://www.se-radio.net/2009/08/episode-143-api-design-with-jim-des-rivieres/

