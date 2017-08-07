Benchmark for new libuv I/O manager
===============================

This benchmark compare libuv I/O manager in stdio to the one in base, aka. mio.

Run test
--------

This benchmark will start a server on your localhost's 8888 port, servering 10000 bytes of zeros.

```
cabal build
./dist/build/libuv/libuv +RTS -s
wrk -c10000 -d10s http://127.0.0.1:8888   

# we don't close connections, so you may have to wait for a while that OS close all connections

./dist/build/mio/mio +RTS -s
wrk -c10000 -d10s http://127.0.0.1:8888   
```
