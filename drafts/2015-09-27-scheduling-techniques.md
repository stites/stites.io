---
date: 1900-01-01
---


Scheduling techniques:

First-come first-serve (aka FIFO):
in a perfect world - this is the best one. Otherwise shorter, burst processes get screwed. The longer-running proccesses aren't as easily affected by the latency since it's a lower percentage of the process' run-time.

Round-robin:
Have a timer go off every 100ms or so. That way shorter processes get the time they need if they are being processed alongside long processes. The only hitch is that if you are working with a large number of short processes, their latency might grow with the number of queued processes.

Layered approach (RR -> FCFS)
