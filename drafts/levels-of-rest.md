---
date: 1900-01-01
---


Dr. Leonard Richardson put together a maturity model that interprets various levels of compliance with RESTful principles, and grades them. It describes 4 levels, starting at level 0. [Martin Fowler has a very good write-up on the maturity model][x].

Level 0: the Swamp of POX - at this level, we're just using HTTP as a transport. You could call SOAP a Level 0 technology. It uses HTTP, but as a transport. It's worth mentioning that you could also use SOAP on top of something like JMS with no HTTP at all. SOAP, thus, is not RESTful. It's only just HTTP-aware.
Level 1: Resources - at this level, a service might use HTTP URIs to distinguish between nouns, or entities, in the system. For example, you might route requests to /customers, /users, etc. XML-RPC is an example of a Level 1 technology: it uses HTTP, and it can use URIs to distinguish endpoints. Ultimately, though, XML-RPC is not RESTful: it’s using HTTP as a transport for something else (remote procedure calls).
Level 2: HTTP Verbs - this is the level you want to be at. If you do everything wrong with Spring MVC, you'll probably still end up here. At this level, services take advantage of native HTTP qualities like headers, status codes, distinct URIs, and more. This is where we'll start our journey.
Level 3: Hypermedia Controls - This final level is where we'll strive to be. Hypermedia, as practiced using the [HATEOAS][hateoas] ("HATEOAS" is a truly welcome acronym for the mouthful, "Hypermedia as the Engine of Application State") design pattern. Hypermedia promotes service longevity by decoupling the consumer of a service from intimate knowledge of that service’s surface area and topology. It describes REST services. The service can answer questions about what to call, and when. We’ll look at this in depth later.
[hateoas]: http://en.wikipedia.org/wiki/HATEOAS
[x]: http://martinfowler.com/articles/richardsonMaturityModel.html
