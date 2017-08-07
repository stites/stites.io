---
layout: post
title: Memory-mapped files
---

Memory-mapped files are a segment of virtual memory that we assign with direct
correlation to some file (or "resource"). Applications can then treat the mapped
portion of the file as if it were primary memory!

It seems like the main upsell is that the operating system has a good api for this
and, in java, we pass control to the operating system to take care of reading and
writing the files. This makes it faster than java IO and also java streaming. On top
of this, if your program crashed just after writing to memory, the OS will take care
of passing on your writes to the file.

Memory-mapped files can also be accessed by more than one process and so it can act
as low-latency shared memory across a multithreaded system.

It seems, however, that this is a little less novel in the haskell community as
performance is already very "close to the metal." It seems like ByteString is already
performant enough with streaming data that most of the general-case uses are
competitive with this feature (from a layman's perspective).

The main thing here is that, in haskell-land, we can consider memory-mapped files as
lazy IO for virtual memory.

Sidebar: In a 32bit machine the region of the file that is mapped can't extend beyond
4GB due to hardware restrictions. Also, memory-mapped file access will result in page
fault if the requested page is not in memory. A lot of unknowns exist as well if the
file is not locked/managaged correctly in your OS.

However in java, depending on whether you are using direct or non-direct byte buffer,
the memory used to load the file is outside of Java heap and resides in shared
memory. This allows two different process to access the file. Usually, you want to
prefer a direct byte Buffer over the non-direct buffer for higher performance.

Note that in haskell, when dealing with your mapped file (or bytestring), you would
definitely want to use one of the list-like packed array types such as `uvector` or
`vector`. As always, benchmark to make sure everything is working as you'd expect.


