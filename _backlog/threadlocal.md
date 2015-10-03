Don't forget that __Threadlocal is not garbage collected__. Also,
there is a creative way of using ThreadLocal where you allocate a specific chunk
of memory to be used as a reusable buffer by a worker thread - depends on
whether or not you are cool with the memory usage, I guess.

On the upside, always remember that ThreadLocal is the best thing to use when
debugging UncaughtExceptionHandlers - the stack frames shut down, but
ThreadLocal remains!

