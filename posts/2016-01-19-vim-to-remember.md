---
layout: post
title: Vim to remember
---

{{ page.title }}
================

It's easy to get stuck doing only what you already know in vim, so here are some from
[vimgolf][0] -- which I highly recommend -- that I've found pretty cool:

 - `.` repeat the insert-mode command from normal-mode
 - `<C-@>` is a null character, apparently - but it also seems to repeat the last
   added character. Like `.` when in insert-mode.
 - ctags will let you use `<C-]>` to _jump to definitions across a project_!
 - also under ctags; use `<C-t>` go to cursor location before the last tag jump

[0]: http://vimgolf.com/
