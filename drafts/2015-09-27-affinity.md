---
date: 1900-01-01
---


Using something like the [Java Thread Affinity][affinity] library
([github][github]), we can acquire a lock on a specific CPU, or reserve
the entire core, for a particular thread. Note that, according to the
repository, reserving a core does not benefit from [hyper-threading][ht] -
if you do reserve an entire core, only one of the CPU twins will be used.

This requires a bit of hardware sympathy, but can increase your
application's performance by taking advantage of cpu caches. While my
job doesn't require me to work at this low of a level, I would say that
the [Heterogeneous Parallel Programming course][het], from the University
of Illinois at Urbana-Champaign (it's a mouthful) is a lot of fun and
definitely worth taking if you are into this kind of thing.

According to a Takipi recommendation, the best way to handle all the
possible variables which might affect your application's performance is
to experiment and test. We can set a process affinity with `taskset`, and
use Java Thread Affinity for the rest.

[affinity]: http://chronicle.software/products/thread-affinity/
[github]: https://github.com/OpenHFT/Java-Thread-Affinity
[ht]: https://en.wikipedia.org/wiki/Hyper-threading
[het]: https://www.coursera.org/course/hetero

