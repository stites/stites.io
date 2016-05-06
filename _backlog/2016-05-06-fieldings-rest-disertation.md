https://www.ics.uci.edu/~fielding/pubs/dissertation/net_app_arch.htm

2.2 Evaluating the Design of Application Architectures

 + The first level of evaluation is set by the application's functional requirements. It makes no sense to evaluate the design of a process control architecture against the requirements of a distributed hypermedia system.

  it is possible to evaluate and compare different architectural designs by
  identifying the constraints within each architecture, evaluating the set of
  properties induced by each constraint, and comparing the cumulative properties of
  the design to those properties required of the application.


2.3.1.1 Network Performance

2.3.1.2 User-perceived Performance

2.3.1.3 Network Efficiency
  through reuse of prior interactions (caching), reduction of the frequency of
  network interactions in relation to user actions (replicated data and disconnected
  operation), or by removing the need for some interactions by moving the processing
  of data closer to the source of the data (mobile code).

2.3 Architectural Properties of Key Interest

2.3.1 Performance

2.3.1.1 Network Performance (throughput)
2.3.1.2 User-perceived Performance (latency)
2.3.1.3 Network Efficiency (caching, replicated data & disconnected operation, move processing closer to source / rpc / "mobile code")

2.3.2 Scalability

Scalability refers to the ability of the architecture to support large numbers of
components, or interactions among components, within an active configuration.
Scalability can be improved by simplifying components, by distributing services
across many components (decentralizing the interactions), and by controlling
interactions and configurations as a result of monitoring. Styles influence these
factors by determining the location of application state, the extent of distribution,
and the coupling between components.

Scalability is also impacted by the frequency of interactions, whether the load on a
component is distributed evenly over time or occurs in peaks, whether an interaction
requires guaranteed delivery or a best-effort, whether a request involves synchronous
or asynchronous handling, and whether the environment is controlled or anarchic
(i.e., can you trust the other components?).

2.3.3 Simplicity

I have chosen to lump the qualities of complexity, understandability, and
verifiability under the general property of simplicity, since they go hand-in-hand
for a network-based system.

Applying the principle of generality to architectural elements also improves
simplicity, since it decreases variation within an architecture. Generality of
connectors leads to middleware [22].

2.3.4 Modifiability

2.3.4.1 Evolvability
2.3.4.2 Extensibility
2.3.4.3 Customizability

Customizability refers to the ability to temporarily specialize the behavior of an
architectural element, such that it can then perform an unusual service.

- A component is customizable if it can be extended by one client of that component's
  services without adversely impacting other clients of that component [50].

- Styles that support customization may also improve simplicity and scalability, since
  service components can be reduced in size and complexity.

- Customizability is a property induced by the remote evaluation and code-on-demand styles.

2.3.4.4 Configurability

Configurability is related to both extensibility and reusability in that it refers
to post-deployment modification of components.

2.3.4.5 Reusability

reduction of coupling (knowledge of identity) between components and constraining the
generality of component interfaces: The uniform pipe-and-filter style exemplifies
these types of constraints.

2.3.5 Visibility (wrt the product, not in the development cycle)
2.3.6 Portability (Software is portable if it can run in different environments)
2.3.7 Reliability (the degree to which an architecture is susceptible to failure at the system level in the presence of partial failures within components, connectors, or data)


CHAPTER 4 Designing the Web Architecture: Problems and Insights

+ Berners-Lee [20] writes that the "Web's major goal was to be a shared information space through which people and machines could communicate."
+ Their machines were a heterogeneous collection of terminals, workstations, servers and supercomputers, requiring a hodge podge of operating system software and file formats.


- Low Entry-barrier:
Since participation in the creation and structuring of information was voluntary, a
low entry-barrier was necessary to enable sufficient adoption. This applied to all
users of the Web architecture: readers, authors, and application developers.

4.1.4 Internet-scale
4.1.4.1 Anarchic Scalability (Clients cannot be expected to maintain knowledge of everything)
4.1.4.2 Independent Deployment (makes the internet fragmented - old and new can also exist)


How "tight-knit" are your teams?

(Bina bioinformaticians) how does the data science team come into play with the development of your product

"On crafting a small paradise in the chaos of the internet"

REST uses a resource identifier to identify the particular resource involved in an interaction between components.

internet built on hypermedia and links between them

Connectors of REST architecture
==============
client      libwww, libwww-perl
server      libwww, Apache API, NSAPI
cache       browser cache, Akamai cache network
resolver    bind (DNS lookup library)
tunnel      SOCKS, SSL after HTTP CONNECT

REST Components
===================
origin          serverApache httpd, Microsoft IIS
gateway         Squid, CGI, Reverse Proxy
proxy           CERN Proxy, Netscape Proxy, Gauntlet
user agent      Netscape Navigator, Lynx, MOMspider

REST Architectural Views
========================
A process view of an architecture is primarily effective at eliciting the interaction
relationships among components by revealing the path of data as it flows through the
system.
 - this is usually what people put in powerpoints: "UserAgent -> {Proxy ->
   Gateway/Firewall -> Origin Server, Origin server, Proxy -> generic tranlator}"

A connector view of an architecture concentrates on the mechanics of the
communication between components. For a REST-based architecture, we are particularly
interested in the constraints that define the generic resource interface.
 - look this up more

A data view of an architecture reveals the application state as information flows
through the components.
 -  Since REST is specifically targeted at distributed information systems, it views
    an application as a cohesive structure of information and control alternatives
    through which a user can perform a desired task.



Methods:
===========
Some new ones form WebDAV

GET - idempotent
POST
PUT - idempotent
PATCH
DELETE

HEAD - GET request without body, for getting metadata
OPTIONS - the HTTP methods that the server supports for the specified URL

TRACE - echoes the received request so that a client (for dig, i think) for seeing intermediate servers
CONNECT - converts the request connection to a transparent TCP/IP tunnel, usually to facilitate SSL-encrypted communication (HTTPS) through an unencrypted HTTP proxy.

RPC seems to be for working with code, "REST" is for content management (CRUD analogy)
RPC seems to use query params - you can't cache these

> The fundamental problem with RPC is coupling. RPC clients become tightly coupled to
> service implementation in several ways and it becomes very hard to change service
> implementation without breaking clients:
http://stackoverflow.com/a/15116562/1529734

