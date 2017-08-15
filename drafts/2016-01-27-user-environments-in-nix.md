---
date: 1900-01-01
---


So I've been a Nix enthusiast for quite a bit, but it's always been just a little too
much work to learn it all on the fly. Today I'll look at setting up a user develop
environment, which is one of the vital day-to-day benefits from nix.

Situation:
 + setting up dev environments for new engineers and avoiding dependency hell

Solution:
 + use a nix development environment

Pros:
 + atomic upgrades/rollbacks of packages
 + consistent and declarative builds
 + "referentially transparent" configurations
 + reliable binaries
 + Nix dev environment-specific: it's lighter than a vm

Cons:
 + learning a new package manager, nix
 + learning a new scripting language, nix

