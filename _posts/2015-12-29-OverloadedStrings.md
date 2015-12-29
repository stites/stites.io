---
layout: post
title: OverloadedStrings
---

{{ page.title }}
================

<p class="meta">29 Dec 2015 - In the air, above Nevada</p>

When the OverloadedStrings pragma is enabled, it changes the definition of `String` from:

    type String = [Char]

to:

    IsString a -> a

with `IsString` defined as:

    class IsString a where
      fromString :: String -> a

This means that strings are now higher-kinded types and, in order to use them, we
need to make them concrete by either (a) explicitly typing what we intend them to be,
or (b) using the `ExtendedDefaultRules` languages extension.

Furthermore, if we are only interested in using a package's `IsString` instance for
use but will not use the package itself, we can import the package and hide all
members to gain access to the `IsString` instance. This would look like the
following:

    {-# LANGUAGE OverloadedStrings #-}
    import Data.ByteString.Char8 () -- only get the orphaned `IsString` instance


