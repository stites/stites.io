---
layout: post
title: Bash Statistics
---

{{ page.title }}
================

<p class="meta">10 Nov 2014 - Redwood City</p>

I had a case where I needed to generate some simple statistics from an indel file which one of our bioinformaticians had procured for me. These files are huge, but instead of asking for the parsed file, I thought it might be fun to do this in bash. I'll walk through an example.

Lets say I have a text file, `filename`, with the following content:

    red apple
    green apple
    green apple
    orange
    orange
    orange

I'm looking for some histogram data to yield a multidimentional array of items and frequency:

    [
      ['red apple', 1],
      ['green apple', 2],
      ['orange', 3],
    ]

First, we need to send our file through `sort` (to put adjacent items together) then `uniq -c` to give counts:

    $ sort filename | uniq -c
      1 red apple
      2 green apple
      3 orange

Now lets rename our `filename` to be `indel.pass.length` and have our labels become indel position (integers). You can stream our desired data from the source into a parsed format:

    sort -n indel.pass.length | uniq -c > indel.pass.length.counts

But we actually want a multidimentional array. Next take all the numbers and insert them into a list of arrays with `sed`.

    sed -E 's/([0-9]+) (-?[0-9]+)/\[\2\,\1\]\,/g' indel.pass.length.counts

Then append and prepend our file with brackets to form our list.

    echo ']' >> $_
    echo '[' > tmp

Finally, move it back into the file we want.

    cat indel.pass.length.counts >> tmp
    cat tmp > indel.pass.length.counts

All together, that looks like:

    sort -n indel.pass.length | uniq -c > indel.pass.length.counts
    sed -E 's/([0-9]+) (-?[0-9]+)/\[\2\,\1\]\,/g' indel.pass.length.counts
    echo ']' >> $_
    echo '[' > tmp
    cat indel.pass.length.counts >> tmp
    cat tmp > indel.pass.length.counts
