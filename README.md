Haskell stdio: haskell standard input and output
================================================

[![Linux Build Status](https://img.shields.io/travis/winterland1989/stdio/master.svg?label=Linux%20build)](https://travis-ci.org/winterland1989/stdio)
[![Windows Build Status](https://img.shields.io/appveyor/ci/winterland1989/stdio/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/winterland1989/stdio/branch/master)

Notice
------

Please head to the [offcial repo](https://github.com/haskell-stdio/stdio). This is the historical experimental repo of stdio, which records many interesting ideas and exploration, and some of these ideas may get revived if needed. Notable branches:

+ `fix_accept3`, this is the final choosen branch for offical repo based on performance.

+ `hs_try_putmvar`, this branch uses the `hs_try_putmvar`(present since GHC 8.2) RTS function to unblock thread. Currently it's slow due to global stable pointer table's lock, and the table always need to be scanned even in minor GC. And my benchmark seems to indicate the malloc/free overhead of `PutMVar` struct is quite large.

+ `strlen-slot-allocator`, this branch uses a least-unused-integer allocator based on a 0-1 slot buffer and C `strlen` function. This allocator saves little memory compare to current linked-list solution, thus we choose the later.

+ `stm-wake-up`, this branch uses `STM` to notify threads which are blocked on waiting uv manager thread to wake up. Under high workload `STM` 's overhead is just too much.

+ `fix_acceptX`, these branches record various ideas on doing socket/pipe accepting, and some of them contain bugs, which are fixed in `fix_accept3`.

+ `new_errno`, this branch tried to unify errno handling with typeclass `IOReturn`. While nice and haskellish as it seems, a more pratical approach is choosen later based on libuv's `uv_translate_sys_error`. and this branch leads to the discovery of a [ghc bug](https://ghc.haskell.org/trac/ghc/ticket/14125) (you can get the basic idea from the trac's issue code).

Most of the other branches are early experiments which are buggy and incomplete, they served to give approximately performance data. Luckily though i got many detail things wrong, but i didn't miss the large elephant: libuv is definitely up to the task.

User Guide
----------

On windows we have bundled libuv source, so not extra steps to be taken.

On \*nix platforms, you should install libuv library first, you can use your distribution's package manager if available, for example:

```
# on debian/ubuntu, make sure to use 1.x
apt-get install libuv1-dev  libuv1

# on MacOS, we recommend brew
brew install libuv

...
```

You can also build libuv from source following the guide [here](https://github.com/libuv/libuv#build-instructions), and modify your `LIBRARY_PATH/CPATH` if necessary. After libuv is in place, installing stdio is as easy as any other haskell packages.

```
cabal install stdio
```
