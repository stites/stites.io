---
layout: post
title: Manage your git branches
---

<p class="meta">24 September 2014 - Redwood City, CA</p>

Thought I'd share this little tidbit — I now routinely clean out my git branches
with:

    git branch | xargs -n 1 git branch -d

Of course, this only removes branches you haven't merged, so it's completely
safe. I set up a cronjob to run to run this in my primary repo about once a
month!
