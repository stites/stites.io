---
layout: post
title: Consensus Algorithms
---

{{ page.title }}
================

You can most easily think of a consensus algorithms arising from the need to
replicate state machines. From this perspective, a state machine would live on a
server, and identical copies of the same state machine would be copied to all other
servers. In this way, even if a server goes down, the state machine can be accessed.
These state machines are typically represented in logs

Logs are the history of actions which the state machine uses to execute commands in
the given order. The logs must be in sync, which turns into the job of theconsensus
algorithm.

Once consensus has been reached on the consistency of the logs/commands and they are
properly replicated, they are executed in order and the output is returned to
clients. In this way, all servers in the cluster give the appearance of a single,
highly reliable state machine.

Consensus algorithms ensure consistency under all non-Byzantine conditions.  This
includes network delays, partitions,and packet loss, duplication, and reordering.

> [Byzantine faults][byzantine], for the uninitiated, are considered the most general
> and difficult class of failures to handle and include any fault which presents
> different symptoms to different observers. Malicious attacks and software errors can
> cause nodes to exhibit Byzantine behavior. See the [Byzantine general's
> problem][byzantine-generals-problem]

In order to maintain consensus, there is a prerequisite that a majority of the
servers re available, functional, and can communicate with clients. So a cluster of
threecan tolerate one failure at a time, a cluster of five can tolerate a failure of
two,and so on. Of course these failed nodes can rejoin the cluster at a later point.

[Clock synchronization is not something which can be assumed in a distributed
system][time-clocks-and-the-ordering]. Paired with extreme packet lag, availability
problems can occur. Because of this, a consesus algorithm can't be dependent on time.

Often, algorithms are written so that nodes to not need to wait for a full consensus
to agree before commands are run. More often, as soon as a majority of the cluster
has confirmed a round of remote procedure calls, the command can execute and slow
servers do not need to impact the overall system's performance.

[byzantine]:https://en.wikipedia.org/wiki/Byzantine_fault_tolerance
[byzantine-generals-problem]: https://www.andrew.cmu.edu/course/15-749/READINGS/required/resilience/lamport82.pdf
[time-clocks-and-the-ordering]:http://research.microsoft.com/en-us/um/people/lamport/pubs/pubs.html#time-clocks


