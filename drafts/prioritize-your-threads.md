---
date: 1900-01-01
---


JVM thread priorities range from 1 (`MIN_PRIORITY`) to 10 (`MAX_PRIORITY`)
with main running at priority 5 (`NORM_PRIORITY`). Take advantage of this
fact and make your application run smarter with a little prioritization.
Note that this isn't a direct correlation with more time from the thread
scheduler, but that's a discussion for another time. For now just
remember this ancedote and prioritize threads accordingly.

__Also don't forget to run the jvm with `-XX:+UseThreadPriorities`__
__otherwise this wont work at all on a Linux box.__

