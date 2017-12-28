---
date: 1900-01-01
---


This is one of those funny fundamentals that most people like to overlook: the
anatomy of a URL. We've had a few issues with some legacy code and encoding, so
I figured it might be interesting to learn about the thought put into one of the
more-common sections of webdev work: the URL.

Generally, there are two things that we actually care about: the Scheme and the
Host address.

Scheme: http
Host address: www.google.com

However there's a whole lot more layers to the onion:
https://sam:secret@www.stites.io:8080/api;p=1?q=2#three

Scheme: https
User: sam
Password: secret
Host address: www.stites.io
Port: 8080
Path: /api
Path parameters: p=1
Query parameters: q=2
Fragment: three
