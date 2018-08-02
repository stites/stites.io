---
layout: post
title: git find my bugs
---

Two different git commands today:

```
$ git rebase -i --exec "make CFLAGS='-Werror' e7509a8c03
```

Runs a linear pass through every commit between e7509a8c03 and HEAD, building the project and stopping the rebase if the build fails. This allows you to fix the commit and continue. You can, obviously, put any command in the `--exec` flag. Good for hard-moding a small segment of git commits.

```
$ git bisect start HEAD e7509a8c03 --      # HEAD is bad, e7509a8c03 is good
$ git bisect run make                      # "make" builds the app
$ git bisect reset                         # quit the bisect session
```

More traditional: git's binary search command to find a bug between e7509a8c03 and HEAD.
