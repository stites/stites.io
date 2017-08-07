---
layout: post
title: What's imported from cabal repl
---

{{ page.title }}
================

<p class="meta">06 Jan 2016 - Belmont, CA</p>

If you have only an executable declared in your cabal file, then when you `cabal
repl` it will import all(?) custom modules depended on in your main file, as well as
a `Main` module which contains your main file.

If you have a library exposed, then your cabal repl will only import your exposed
modules. You also won't get access to the magically generated `Main` module after you
drop into `ghci`. Even if you export both libraries and executables, only the rules
of the library will be imported.

You can sidestep this with: `cabal repl <executable name>` -- however you should also
keep in mind that your named executible may have a colliding name with the name of
the library (found in `name` at the top of your cabal file).

If you want to import a specific module into your cabal repl, you can do the same
trick:

    cabal repl <module name>

However you can still wind up with a namespace collision. Since you cabal allows us
to import files without needing to specify the full or relative path, you can import
a file (containing the module you want) and hope that cabal will fuzzy-match what you
are expecting.

So if you don't titlecase the module name, say if you enter `hello`, cabal may think
that you are referencing a file and need further disambiguation between the module
name, `Hello`, and the file that contains it, `hello.hs` or `Hello.hs`. You may, of
course, also pass the full or relative path as a means of disambiguation.

