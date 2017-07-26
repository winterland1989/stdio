Benchmark for new libuv I/O manager
===============================

This benchmark compare libuv I/O manager in stdio to the one in base, aka. mio.

Run test
--------

Start a server on your localhost's 8081 port, server the file in diskIO bench folder, for example

```
npm i http-server -g
cd ../diskIO 
http-server -p 8081
```

then come back, and

```
cabal build
./dist/build/libuv/libuv +RTS -s
./dist/build/mio/mio +RTS -s
```
