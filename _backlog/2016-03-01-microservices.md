We're seeing an increased understanding of the pros and cons of microservices. In
this newsletter, I'll list the ones I've gathered from a dozen other newsletters
where I've previously extracted microservice performance tips.

Pros:

Decoupled systems have higher fault tolerance and are easier to understand and fix.
Improved fault isolation. Ability to upgrade components independently. Easier to
scale as you can scale only those services that need to be scaled. Encourages better
handling of partial failures (as you can more clearly see interconnectedness). Fast
starts. The ability to replace a service completely with a new implementation.
Separate optimised data stores for each microservice. Reduced complexity. Less likely
to use (and need) huge machines; can use cheaper hardware.

Cons:

Consistency issues across data stores. All the overheads of a service are multipled
across the system, as each microservice needs to independently throttle, timeout,
circuit break, manage connection and thread pools, pipeline requests asynchronously.
Too much decomposition can cause too many network hops, excessive runtime overheads,
and a complex interlinked difficult to understand system. Increased communication
failures. Testing a distributed system is harder. Additional latency from additional
hops. Inefficiencies from multiple processes duplicating their uses of similar
resources (eg JVM GC threads). Distributed service issues: split brains, dropped
packets, slow networks, other JVMs hitting GC pauses. You need to be much more
rigorous in decoupling your components and there is additional deployment and
distributed monitoring complexity.

Many of these cons are related to having distributed systems, but if you will be
having a fault tolerant clustered system with failover capability you are likely to
have the same issues anyway, so for large systems there are fewer cons than it seems
here. And increased simplicity of understanding the system may override everything,
as the real bottleneck is the people developing and running the system.
