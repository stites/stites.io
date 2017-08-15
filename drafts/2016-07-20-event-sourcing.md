---
date: 1900-01-01
---


Event sourcing

A very simple concept. The idea that state is actually just a state machine with a stream of events.

Things that use event sourcing intrinsically include data migrations and version control systems.

event sourcing is great because you get safety of slip-ups, you can rewrite the
global state, and synching a cluster is a solved problem via paxos or raft (so
we get strong garuntees for free). You just have to maintain the log/event
stream, but that is also a solved thing.

"It's basically a functional, streamed, stateless, database - functional because your state is a fold over a stream of events." - Dan.

to actually build one: look into kafka (logging), samza (logging), leveldb (what dan uses), rockdb (from a talk)

 + How does ES work at scale? Cluster/shard your journal
   - Dan: "You can cluster your journal. Say, your collection now contains 100
     million events, you can split it into multiple journals, each consumed by
     an individual actor and each actor can run on an independent instance.
     Basically sharding." Kafka also looks a lot like database replication.

 + How do you deal with massive replay scenarios? You can also establish a baseline state that you read from.

Space issue
===========
Sam Stites [12:47 PM]  
so, just to reiterate, if you do work with confined space requirements it’s possible to do event sourcing, check-pointing, and throwing away any events prior to the checkpoint?

Dan Chao [12:48 PM]  
you never want to truly throw away events

Sam Stites [12:48 PM]  
in theory, though?

Sam Stites [12:50 PM]  
just cause memory and space requirements were something I had to deal with at bina

Dan Chao [12:48 PM]  
if you’re confined by space

[12:49]  
like, let’s say you can only store 10 events and that’s it

Dan Chao [12:50 PM]  
you build your state through programmatically figuring out how these events change your state

[12:50]  
and if you ever change the way you consume these events

[12:51]  
you need to replay the whole thing before you can snapshot your state

Sam Stites [12:52 PM]  
you can’t do something like `SNAP_A -> E37 -> E38 -> E39 -> HEAD`, then later replay and start from `SNAP_A`? (edited)

Dan Chao [12:53 PM]  
yeah, let’s say the event is `POST_DELETED`

[12:53]  
and suddenly we decide we don’t want people to be allowed to delete their posts

[12:54]  
we’d write some code that ignores `POST_DELETED` events

[12:54]  
however, `SNAP_A` wouldn’t contain any posts

[12:54]  
we have to replay from the beginning to build the correct state of the world

Sam Stites [12:54 PM]  
!!!

[12:55]  
that’s pretty cool

[12:56]  
but, I see ES as a really wonderful way to iterate a product: you could mess up the state/schema in prototyping phase, then fix things later

Dan Chao [12:56 PM]  
yeah

Sam Stites [12:56 PM]  
snapshot, ditch the old prototype state, and continue developing

Dan Chao [12:56 PM]  
it gives you a pretty great safety net if you screw up

Sam Stites [12:56 PM]  
nice

[12:56]  
is that a common situation, though? global state machine rewrites?

Dan Chao [12:57 PM]  
yeah, somewhat common

Sam Stites [12:57 PM]  
crazy!

Dan Chao [12:58 PM]  
usually not as dramatic as like reinstating deleted posts

[12:58]  
usually it’s more like, you add a new property to some model

Side note
==============

> "event sourcing (enterprise term) == stream processing (internet SW term)"
>   \- Martin Kleppmann, Event Sourcing and Stream Processing at Scale

