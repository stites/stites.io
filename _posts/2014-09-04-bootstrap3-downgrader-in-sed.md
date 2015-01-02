---
layout: post
title: writing a small bootstrap 3 downgrader in sed
---

{{ page.title }}
================

<p class="meta">04 Sept 2014 - Redwood City, CA</p>

Some notes on `sed`:
always use `-E` for the more 'standard' regex
always use single quotes for the regex so that you don't have to escape metacharacters
sed doesnt allow for non capture groups
you can execute a script with `-e` (not nessecary for a regex)
chop up your regexs into smaller ones and use `-e` to seperate out regexs by `;`s
you can use -i to change a file on the fly. use `.bak` for a backup as you write or `. ` for none

example:

    sed -E \
    -e 's/(col-sm-)|(col-md-)([0-9]+)/span\3/g;s/span([0-9]+) span[0-9]+/span\1/g' \
    -i.bak <template>.html

Running this agains multiple files requires you to find all files:

    find *.html -type f -print0

`-print0` always evaluates to true and prints to stout followed by an ASCII NUL character (`\0`).

then pipe each result into `xargs` - `-0` takes `\0` as a seperator, `-n` is the number of arguments the utility takes:

    xargs -0 -n 1 < <utility> <utility args> >

Putting this all together:

    find *.html -type f -print0 | \
    xargs -0 -n 1 \
    sed -E -i.bak \
    -e 's/(col-sm-)|(col-md-)([0-9]+)/span\3/g;s/span([0-9]+) span[0-9]+/span\1/g'

And we've created our bootstrap 3 downgrader!
