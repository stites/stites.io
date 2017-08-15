---
date: 1900-01-01
---


we write a custom RDD graph (a *r*ound-*r*obin *d*atabase graph) which uses the
time-series data collected by `rddtool` to plot some simple graphs of system
resources. The tool runs on a "round-robin" in the sense that only a certain amount
of data is collected before it wraps back and starts over again, keeping the
collected data to a fixed amount.

So we actually use the tool not to graph, but to aggregate data in an efficent
manner. We do a pretty interesting thing where we'll access `rddtool` directly from
the terminal, construct the command to run in java, then run a command in a seperate
process to output data we want to plot, collect the stream of data from stdin, and
finally construct a graph from the data we need.


