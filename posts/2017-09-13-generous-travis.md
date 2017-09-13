---
title: (Note) A more generous Travis-CI
---

I was hitting an out-of-memory error in travis and found that adding `sudo:required` to a .travis.yml will switch your build to use their standard infrastructure (currently 3GB ram on 2 cores, according to the FAQ). Without this, travis uses containers and hitting upper limits for any build becomes more of a problem.

