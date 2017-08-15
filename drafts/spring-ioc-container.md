notes from reading about Spring's "The IoC container". [Full docs here][docs].
+ invertion of controll is synonymous with Dependency injection! I guess it's
  always made sense, but i never hear anyone talk about IoC anymore.
Spring's IoC container looks something like:

Business logic -->|------------------|    |-----------------------|
                  | Spring Container | -> |Fully configured system|
Config medatada-->|------------------|    |-----------------------|

`org.springframework.context.ApplicationContext` is the Spring IoC container
and does the instantiating, configuring, and assembling of beans.

[docs]: http://docs.spring.io/spring/docs/current/spring-framework-reference/html/beans.html#beans-factory-nature
