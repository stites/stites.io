- writer is either too lazy or too strict for whatever you're working on.
- it has a tendency to accumulate unevaluated thunks. Thus, memory leaks.
- you can't retrieve any of the logged values until the computation is complete.
  - http://www.haskellforall.com/2014/02/streaming-logging.html
  - Thus, inappropriate for logging long-running or ongoing programs (or any prog)

