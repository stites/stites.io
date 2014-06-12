---
layout: post
title: `git reset --hard` on one file
---

{{ page.title }}
================

<p class="meta">12 June 2014 - Redwood City, CA</p>

I feel like, at a certain point in time, I used to be able to run the command `git reset --hard path/to/file`. However this might have been me only working with single files, because now I frequently get the error:

#### `fatal: Cannot do hard reset with paths.`

This one has been a tiny thorn in my side - perhaps ever since I upgraded to git 2.0 - or maybe it's a problem cause by extending `git` with `hub` (PS get `hub` here and __now__ if you haven't heard of it- maybe I'll write something on this later). Most likely it was just my insanity.

The solution is:

    git checkout -- path/to/file

This performs a file-specific `git checkout`. Exactly what I've been looking for. Taking a closer look, you can also perform `git checkout <branch_name> -- <paths>` to get copies of files from different branches.

As always, check out the man files (`man git-checkout`) for more!
