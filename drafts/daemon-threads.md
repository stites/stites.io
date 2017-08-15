---
date: 1900-01-01
---


Use daemon threads. Use them when your process is non-vital. Use them
when you do not want to worry about closing the thread properly. Use
them to shut down your program quickly. _Use them sparingingly and
thoughtfully_.

Backdrop: you have user threads, which run in the foreground, and daemon
threads, which run in the background. Since threads inherit from their
forked source and the main thread is a User-thread by default, it's
likely that you are using only foreground threads. Daemon threads are
pretty nifty in that when they are the only threads running in your
appliction, the process will shutdown anyways.

_But this is not a graceful exit._ A daemon thread will not invoke its
`finalize()` block, and the stacks are not unwound. So don't just use
daemon threads mindlessly! Use them when _they don't hold any resources_
and when _they don't do any IO_. Also, make sure you add a [shutdown hook
to the JVM Runtime][1] to ensure that you are performing a graceful exit.

[1]: http://stackoverflow.com/questions/8663107/how-does-the-jvm-terminate-daemon-threads-or-how-to-write-daemon-threads-that-t

