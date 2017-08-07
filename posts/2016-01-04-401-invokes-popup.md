---
layout: post
title: Building json APIs with status code 401
---

{{ page.title }}
================

"401 Unauthorized" invokes a browser popup as it's the default behaviour on almost
every browser (that I know of). You can either return a non-401 status code, or
change the Authorization header from _Basic_ or _Digest_ to something like _Json_.

