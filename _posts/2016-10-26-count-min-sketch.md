---
layout: post
title: Count-Min Sketch
---

{{ page.title }}
================

A lesser known probabalistic data structure (alongside bloom filters and the
hyperloglog) includes the Count-Min Sketch. To cite Cormode and Muthukrishnan in
their most recent paper "Approximating Data with the Count-Min Data Structure"
[[1][1]] to illustrate the reason behind sketches

> Consider a popular website which wants to keep track of statistics on the
queries used to search the site. One could keep track of the full log of queries
[but the log will] become very large. This problem is an instance of the count
tracking problem. [..] Notice that in this scenario, we can tolerate a little
imprecision. In general, we are interested only in the queries that are asked
frequently. So it is acceptable if there is some fuzziness in the counts [which
yields] more efficient and lightweight solutions. This tradeoff is at the heart
of sketches.

The API for count tracking includes two methods: `update (item, counts)` and
`estimate (item)`. This could be done with a hashmap, but the size may become
unweildy. The Count-Min Sketch data structure primarily consists of a fixed
array of counters, of width w and depth d. all counters are initialized to
zero, and each array is associated with a different hashing function. The
hashes don't need to be particularly strong (ie, they don't have to be
cryptographic).

Update operations are straight forward: each row runs its corresponding hash
function to find the counter, then the counter is updated with the number of
counts in the function call. Estimates are similar: each hash is applied and
the counters are found. The minimum of all counters found then becomes the
returned value. This works because the hash functions spread out collisions.

The more generic verion of Count-Min Sketches is to identify "Heavy-Hitters."
This adds another function to our API: `HH (k)` which returns the set of items
which have a frequency as large as `1/k` of the overall frequency. This can
already be addressed with our current information, however we can speed up this
query by additionally storing the frequencies of _groups of items_.


Outstanding questions:
- Why is taking the minimum acceptable as opposed to, say, the max or average?


Other Sketches:
- Bloom Filter (simple membership)
  + massive database queries
- AMS Sketch (represents high dimensional vectors in small space)
  + machine learning
- Distinct Sketches (estimate the number of different elements within the set)
  + unique visitors


[Also][official], [also][streaming], [also][original].

[official]: https://sites.google.com/site/countminsketch/
[streaming]: http://bravenewgeek.com/tag/count-min-sketch/
[tutorial]: https://perma.cc/W8YV-STTW
[original]: https://perma.cc/6K8H-RLUK
[1]: https://perma.cc/MS6G-MDTF

