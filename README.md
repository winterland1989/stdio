stdio: standard input and output
================================

This library is an effort trying to improve and standardize Haskell's IO interface with pack data types.

+ A faster/simpler vector type, which packed bytes type built on.
+ Binary and textual `Parser` and `Builder` for compact bytes.
+ A UTF-8 based text type for text process.
+ A simper `Hanlder` for IO, include file and network module.
+ A compact `FilePath` type.

Roadmap
-------

+ A unified vector type. (80%)
+ Unpinned bytestring based on vector. (80%) 
+ `Foregin` module for bytes. (0%)
+ IO system for `Bytes`, new `Handler` design. (10%)
+ IO system for `Bytes`, file and network part. (20%)
+ A compact `FilePath` type. (0%)
+ `Builder` for `Bytes`, both binary and textual. (50%)
+ `Parser` for `Bytes`, both binary and textual. (0%)
+ Basic UTF-8 text processing (0%)
+ Extend UTF-8 text processing, (normalization, unicode case-mapping, etc.) (0%)

Join in!
--------

This project need lots of effort than it seems, any contributions is welcome! Feel free to

+ Discuss design.
+ Report bugs.
+ Write implementations.

FAQ
---

+ Is this a custom `Prelude` thing?

No, this package only target a very limited focus: packed data types and IO interface, the vector, bytes, text, filepath, parser and builder parts are here to support. Anything out of this scope should not be included, for example, a better `Num` tower.

+ Why not use bytestring and text?

Bytestring is based on `ForeignPtr` which always allocate pinned memory, which can be slow and bring memory fragmentation. stdio use `ByteArray` based representation will leaverage RTS's memory allocator. 

`Text` in stdio are UTF8-encoded.

+ Why not use vector?

One key design point is that stdio DO NOT use implicit stream fusion(both vector/bytes and text), but we provide optimized `pack/unpack` implementations which is good consumer/producer from foldr/build fusion perspective. which means if you need elimitate intermediate data structures you should do something like:

```haskell
pack . map YYY . filter XXX . unpack
```

`pack/unpack` has a little constant overhead but if the processing chain is long, it may be paid off. 

The reason for not introduce implicit fusion is that packed datatype is designed for sharing, which will destroy any form of fusion. On another hand, list in based is very bad at sharing but nice for processing. So in stdio the choice is simple: we only provide packed operations, `pack/unpack` when you want fusion to happen.

+ Why not foundation?

The array type in foundation is a sum type, and carry extra word for pinned status. Which is unreasonable. And the abuse of type class/family is only a comlication IMO.

+ What about FFI with bytes in stdio?

You just have to understand a little bit about GHC's RTS: an unsafe FFI call works like a fat primitive machine code which stops GC. So basically you can pass `ByteArray` to any unsafe FFI calls with the help of `UnliftedFFITypes` extension. On the other hand, if you want to make a safe FFI call, use `isPrimArrayPinned` to decide if you want to allocate a pinned copy.

I plan to add `Foregin` module to help, which is not implemented yet.
