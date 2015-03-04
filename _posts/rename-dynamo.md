---
layout: post
title: Dynamo
---

{{ page.title }}
================

<p class="meta">March 08, 2015 - Mountain View, CA</p>

Starting to read CS papers from Fogus' post, [10 Technical Papers Every
Programmer Should Read (At Least Twice)][papers]. I'll be going through
these papers and writing synopsis and general thoughts.

## Dynamo: Amazon's Highly Available Key-value Store

### Abstract
Reliability at massive scale is a challenge at Amazon.
as with big companies - small outages have large implications (usually financial, also
trust).

This paper is the design and implementation of Dynamo - a highly available key-value storage system which a lot of amazon core is built on.
provides "always on" experience.
sacrifices consistency "under certain failure scenarios" for strong availability.
Makes use of object versioning and application-assisted conflict resolution in a manner that provides a novel interface for developers to use.

### TOC

+ 1. purpose
+ 2. introduction
- 3. background
- 4. related work
- 5. system design
- 6. implementation
- 7. experiences and insights from implementing in production.
- 8. conclusion

### 1. Purpose
for the research community to see how different techniques can be combined to provide a single, highly-available system.
dynamo is a eventually-consistent storage system which can be used in production with demanding applications.
also dynamo shows how you can tune techniques to meet requirements of production systems with very strict performance demands.

### 2. Introduction
Amazon needs:
+ large usage "tens of millions" at peak load
+ highly distributed "tens of thousands of servers across the world"
+ requires high performance, reliability, efficiency and scalability

Reliability is one of the most important because small outages have significant financial and customer relations problems.

Lesson learned - reliability and scalability of a system is dependent on how its application state is managed.
Result - amazon highly desentralized, loosely coupled, service oriented.
Result - hundreds of services.
Implication - need for available storage.
More detail - customers should see and view data even if disks are failing, network routes are flapping or data centers are being 'destroyed by tornados'
ie - always R/W and data on multiple data centers.

dynamo is used to manage the state of services hat have very high reliability requirements and need tight control with trade offs between availability, consistency, cost-effectiveness and performance.

basically it's primary-key access to a data store. something like seller lists, shopping carts, customer preferences... etc. stuff that relational databases arent good with.
EXAMPLE - at amazon dynamo is used to scale peak loads efficiently for shopping cart

the general idea is that dynamo uses well-known techniques together in a tandem to acheive scalability and availability. A list of them include:
+ data being partitioned and replicated using consistent hashing.
+ consistency facilitated by using object versioning.
+ consistency among replicas during updates is maintained by a quorum-like technique and a decentralized replica synchronization protocol.
+ it uses a gossip-based distributed failure detection and membership protocol.
+ it's completely decentralized and, thus, needs minimal manual administration.
+ storage nodes can be added or removed from dynamo without requiring any manual partitioning or redistribution.

### 3. Background
Amazon has standard needs of a large-scale production environment.
Usually you see RDBMS however this sucks for a lot of patterns
- most store and retrieve data by primary key and don't require complex querying and management found in RDBMS
- also means costs in the form of expensive hardware (?) and highly skilled personnel.
there have been a lot of advances, but it's still not easy to scale out a database or use smart partitioning schemes for load balancing.(LVM?)
-> dynamo is simple and solves all of this.

#### System Assumption and requirements
+ query model: simple read and write operations to a data item that is uniquely identified by a key. State is stored as binary objects (blobs) ided by unique keys. no operations span multiple data items and there is no need for a relational schema.
    - as an aside - dynamo targets applications that need to store relatively small objects (usually <1MB)

+ ACID properties (Atomicy Consistency, Isolation and Durability) usually means more reliability at the cost of less availability (industry and academia proven) - dynamo targets models with weaker consistency.

+ Efficiency: needs to run on commodity hardware infrasturcture. state access plays a crucial role in service operation so the storage system must be capable of meeting such stringent SLAs (next). services must be able to configure dynamo such that they consistently achieve their latancy and throughput requirements. tradeoffs are in performance, cost efficiency, availability, and durablility guarantees.

operation environment assumbed to be 'non-hostile'
security is not considered an issue.

#### Service Level Agreements(SLAs)
this is between a client and service. something like "get me responses in 300ms 99.9% of the time for a peak load of 500requests per second"

this happens at amazon and is pretty important with the level of SOA, decentralization, and the high number of dependencies.

a common approach to handle performance-oriented SLAs is to describle them in terms of mean/avg/median/variance. Amazon has found that this only satisfies the majority and so they work with the 99.9-th percentile of the distribution. 99.9% was chosen after a cost-benefite analysis.

Amazon engineering and optimization efforst are not focused on averages - more, they are targeted at this percentile. Load balancing selection of writing coordinators ( and more) are purely at controlling performance at this mark.

** Storage systems play an important role in establising a service's SLA because state management is one of the primary components of it. Thus - one of the main considerations for Dynamo is to _give services control over ther system properties such as consistency and durability_ and to _let services make their own tradeoffs between functionality, performance and cost effectiveness.

#### Design considerations


[papers]: http://blog.fogus.me/2011/09/08/10-technical-papers-every-programmer-should-read-at-least-twice/
