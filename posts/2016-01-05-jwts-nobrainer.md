---
layout: post
title: JWTs are a no-brainer
---

Why? Well they're self signed, so no storage is involved. Even if you are integrating
systems, you don't have to worry (too much) about "merging jwt-based authentication
systems" -- only then _data_ that is contained on the tokens; but that was always
going to be an issue.

Even taking into account the fact that user information may be stored in your tokens
isn't a problem since you'd need the server's private key to decrypt them.

#### Why jwts are a pain in the butt:

Using them to store user information. When a user updates their account information,
jwts need to be updated as well. Now, if we weren't using jwts and everything was
Session-based, we could do something where we update the security context while keeping
the user online; piece of cake! But, if you're using jwts, you'd have to invalidate that
token and all information is stored on there so everything gets fucked!

---

Long story short. Use jwts but don't store everything under the goddamn sun in your
tokens. Plus, who in their right mind would want to send a "204 No Content" with a
super-massive json header?



