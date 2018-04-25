Benchmark for new libuv I/O manager
===============================

This benchmark compares following I/O multiplexers:

+ current one in base, aka. mio     

This is an M:N multiplexers, each OS thread(capability in GHC rts) contain an `uv_loop` poller, and one haskell thread to mananger the poller.

+ libuv I/O manager in stdio

This is an M:N multiplexers just like mio, but use libuv as OSes abstraction. 

+ golang's netpoller

This is an M:N multiplexers, but golang rts only start one extra thread doing I/O multiplex, M user threads on N OS threads all request I/O scheduling from this poller thread.

+ nodejs cluster

This is a single threaded multiplexers, but use multiples process to take advantage of multiple CPU.


Run test
--------

This benchmark will start a server on your localhost's 8888 port, read some input(and ignore them), them servering 10000 bytes of zeros in HTTP protocal, so that you can use HTTP benchmark tools such as `siege` or `wrk` to bench.

You should adjust your system's fd limit before running benchmark in case of running out of fd.

```
cabal build

# mio
./dist/build/mio/mio +RTS -s

# stdio
./dist/build/libuv/libuv +RTS -s

# golang
go run golang/main.go

# nodejs
node nodejs/main.js

# wrk
wrk -c1000 -d10s http://127.0.0.1:8888   

# siege
siege -c 1000 -r 10 http://127.0.0.1:8888 

...
```
