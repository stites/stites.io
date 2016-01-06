---
layout: post
title: Browse GHCi
---

{{ page.title }}
================

<p class="meta">05 Jan 2016 - Redwood City, CA</p>

Use `:browse <module>` to view modules which you haven't yet imported. For example:

    Prelude> :browse Data.Bool
    bool :: a -> a -> Bool -> a
    (&&) :: Bool -> Bool -> Bool
    data Bool = False | True
    not :: Bool -> Bool
    otherwise :: Bool
    (||) :: Bool -> Bool -> Bool

