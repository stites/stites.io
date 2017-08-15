---
date: 1900-01-01
---


Sixth normal form (6nf)
from relational database theory can be used in two different ways.

sixth normal is a normal form for temporal databases based on an extension of
the relational algebra.

Quote from "Time and relational theory":
  A relvar R [table] is in sixth normal form (abbreviated 6NF) if and only if
  it satisfies no nontrivial join dependencies at all - where, as before, a
  join dependency is trivial if and only if at least one of the projections
  (possibly U\_projections) involved is taken over the set of all attributes
  of the relvar [table] concerned.[Date et al.]

Sixth normal form is intended to decompose relation variables to irreducible
components. Though this may be relatively unimportant for non-temporal
relation variables, it can be important when dealing with temporal variables
or other interval data. For instance, if a relation comprises a supplier's
name, status, and city, we may also want to add temporal data, such as the time
during which these values are, or were, valid (e.g., for historical data) but
the three values may vary independently of each other and at different rates.
We may, for instance, wish to trace the history of changes to Status.

https://en.wikipedia.org/wiki/Sixth_normal_form
http://stackoverflow.com/questions/4824714/would-like-to-understand-6nf-with-an-example


