Benchmark for different Disk IO
===============================

Unlike socket I/O which is event based in almost all modern OS, event based disk I/O is quite fragile. For example to provide an async interface for disk files, libuv use a thread pool, the same story goes with glibc's AIO support. The complexity lies in that there're so many layers of buffering between user space programs and the hardware, and even OS provides a true event based interface for programmers, they probably should not use them since programmers can't see overall disk activities like OS do:
They'd better start multiple threads and leave disk I/O scheduling to OS.

In haskell we trust I/O manager to do I/O scheduling. In the case of disk I/O, the thing get quite messy: `select/poll/epoll` will report disk files are readable and writable any time we ask them, then we do a blocking read or write immediately which are unsafe calls. This is not good because unsafe calls will block GHC's runtime, they also make GC's latency unpredictable.

On the other hand, if we only use safe FFI to make disk I/O, we're simply forking new OS threads, which can be too many. And the safe FFI overhead is taxing us. But we may get better runtime characteristics.

I also add a limited pool version, which works by limited all unsafe I/O operastions to only 3 threads, and use `MVar` to sync, which mimic a thread pool: There're at most 3 threads performing disk I/O, so the whole system is not blocked with `-N4`. In practice the pool size should be adjust by capacity number though.

The choice is very hard to make, because disk I/O operastions is very unpredictable in duration, This test is to test each of the options we have, and try hopefully to find the best one.

Run test
--------

```
cabal build
time dist/build/unsafe-ffi/unsafe-ffi 1k
time dist/build/select/select 1k
time dist/build/safe-ffi/safe-ffi 1k
time dist/build/thread-pool/thread-pool 1k
time dist/build/unsafe-ffi/unsafe-ffi 1m
time dist/build/select/select 1m
time dist/build/safe-ffi/safe-ffi 1m
time dist/build/thread-pool/thread-pool 1m
time dist/build/unsafe-ffi/unsafe-ffi 10m
time dist/build/select/select 10m
time dist/build/safe-ffi/safe-ffi 10m
time dist/build/thread-pool/thread-pool 10m

# clean up
rm 1k-*
rm 1m-*
rm 10m-*
```
