---
layout: post
title: Eclipse hangs on git status computation
---

<p class="meta">Sept 27, 2015 - Redwood City, CA</p>

If eclipse starts hanging with the process "Computing git status for repository..."
remove the following files:

    rm -r $ECLIPSE_WORKSPACE_DIR/.metadata/.plugins/org.eclipse.core.resources/.projects/*/org.eclipse.egit.core
    rm -r $ECLIPSE_WORKSPACE_DIR/.metadata/.plugins/org.eclipse.core.resources/.projects/*/.indexes/properties.index

