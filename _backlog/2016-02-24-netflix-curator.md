We use a lot of netflix OSS at Bina. I've recently started working on some backend
tickets and have to learn about curator. Here's what I gather:

Zookeeper can be difficult to use correctly, so curator is like the "best practices"
API for it.

[0]: http://techblog.netflix.com/2011/11/introducing-curator-netflix-zookeeper.html

From [Netflix blog][0]:
Connection Issues:
+ Initial connection: the ZooKeeper client does a handshake with the server that
  takes some time. Any methods executed synchronously against the server (e.g.
  create(), getData(), etc.) will throw an exception if this handshake hasn't
  completed.

+ Failover: if the ZooKeeper client loses its connection to the server, it will
  failover to another server in the cluster. However, this process puts the client
  back into "initial connection" mode.

+ Session expiration: there are edge cases that can cause the ZooKeeper session to
  expire. Clients are expected to watch for this state and close and re-create the
  ZooKeeper instance.

Recoverable Errors:

+ When creating a sequential ZNode on the server, there is the possibility that the
  server will successfully create the ZNode but crash prior to returning the node
  name to the client.

+ There are several recoverable exceptions thrown by the ZooKeeper client. Users are
  expected to catch these exceptions and retry the operation.

Recipes:

+ The standard ZooKeeper "recipes" (locks, leaders, etc.) are only minimally
  described and subtly difficult to write correctly.

+ Some important edge cases are not mentioned in the recipes. For example, the lock
  recipe does not describe how to deal with a server that successfully creates the
  Sequential/Ephemeral node but crashes before returning the node name to the client.
  If not dealt with properly, dead locks can result.

+ Certain use cases must be conscious of connection issues. For example, Leader
  Election must watch for connection instability. If the connected server crashes,
  the leader cannot assume it is safe to continue as the leader until failover to
  another server is successful.

More from netflix
----------------
`curator-client` - A replacement for the bundled ZooKeeper class that takes care of
some low-level housekeeping and provides some useful utilities

`curator-framework` - The Curator Framework is a high-level API that greatly
simplifies using ZooKeeper. It adds many features that build on ZooKeeper and
handles the complexity of managing connections to the ZooKeeper cluster and
retrying operations.

`curator-recipes` - Implementations of some of the common ZooKeeper "recipes". The
implementations are built on top of the Curator Framework.

Curator is focused on the recipes: locks, leaders, etc. Most people interested in
ZooKeeper don't need to be concerned with the details of connection management, etc.
What they want is a simple way to use the recipes. Curator is directed at this goal.

Curator deals with ZooKeeper complexity in the following ways:

+ Retry Mechanism: Curator supports a pluggable retry mechanism. All ZooKeeper
  operations that generate a recoverable error get retried per the configured retry
  policy. Curator comes bundled with several standard retry policies (e.g.
  exponential backoff).

### We write our own exponential backoff - why? Perhaps because we _are_ using it,
### but we override a few things which makes it only _look_ like custom
### implementations.

+ Connection State Monitoring: Curator constantly monitors the ZooKeeper connection.
  Curator users can listen for state changes in the connection and respond
  accordingly.

+ ZooKeeper Instance Management: Curator manages the actual connection to the
  ZooKeeper cluster using the standard ZooKeeper class. However, the instance is
  managed internally (though you can access it if needed) and recreated as needed.
  Thus, Curator provides a reliable handle to the ZooKeeper cluster (unlike the
  built-in implementation).

+ Correct, Reliable Recipes: Curator comes bundled with implementations of most of
  the important ZooKeeper recipes (and some additional recipes as well). The
  implementations are written using ZooKeeper best practices and take account of all
  known edge cases (as mentioned above).

+ Curator's focus on recipes makes your code more resilient as you can focus strictly
  on the ZooKeeper feature you're interested in without worrying about correctly
  implementing ZooKeeper housekeeping requirements.

