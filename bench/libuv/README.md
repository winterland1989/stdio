Benchmark for different Disk IO
===============================

This benchmark compare libuv I/O manager in stdio to the one in base, aka. mio.

Run test
--------

Start a server on your localhost's 8081 port, then

```
cabal build
./dist/build/libuv/libuv +RTS -s
./dist/build/mio/mio +RTS -s
```
