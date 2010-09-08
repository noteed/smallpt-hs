smallpt-hs
==========

This is a port of [smallpt](http://www.kevinbeason.com/smallpt/), a global
illumination path tracer written in 99 lines of C++. The port is written in
99 lines of Haskell.

There are two major differences: performance and argument parsing. The Haskell
code compiled with GHC 6.12.1 is about 4.5 times slower than the C++ version.
(I only tested on my anemic Atom N450-powered netbook...) The C++ code takes an
optional argument, which should be a integer greater than 4.

Give it a spin
--------------

The complete package contains more than the 99 lines smallpt-hs.hs file. There
are other variants with a criterion wrapper to benchmark the code. The cbits
directory contains the original smallpt.cpp file modified to be expose the main
entry point as a C symbol callable from Haskell via FFI.

Every attempt so far use the vector package. The 99 lines file is derived from
Smallpt/Mutable.hs. The Storable code doesn't improve the performance and is
less convenient to use. The Unboxed code lose in performance because the Vec
type can't benefit from an explicit unpack pragma.

If you can make the code faster or easier to read, please let me know!

License
-------

Although the LICENSE file is a BSD3 license, I should contact Kevin Beason (the
original author of the C++ code) to make sure it is ok. The linked site above
has the original LICENSE.
