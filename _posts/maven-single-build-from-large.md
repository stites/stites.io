mvn -pl portal/frontend -am package -DskipTests=true

and do this from the *ROOT* of seqalto. 

From: http://zeroturnaround.com/rebellabs/your-maven-build-is-slow-speed-it-up/

use -pl combined with -am to build a single module and it's dependencies.

Aside from eclipse being magical, for everyone else, you can simplify your
installs with 

    mvn -pl genomics/core -am install (-DskipTests=true)

will build only genomics/core and it's few dependencies. also the -T<N>C
argument is nice, didn't know about the C suffix.

