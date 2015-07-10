hLibsass
========
[![Build Status](https://travis-ci.org/jakubfijalkowski/hlibsass.svg?branch=master)](https://travis-ci.org/jakubfijalkowski/hlibsass)
[![Hackage](https://img.shields.io/hackage/v/hlibsass.svg)](http://hackage.haskell.org/package/hlibsass)
[![Hackage deps](https://img.shields.io/hackage-deps/v/hlibsass.svg)](http://packdeps.haskellers.com/reverse/hlibsass)

hLibsass is a low-level binding to [LibSass](https://github.com/sass/libsass "LibSass"). It provides basic types used by the library along with a FFI interface to the C API.

It is available on [Hackage](http://hackage.haskell.org/package/hlibsass).

### Documentation

This library is (mostly) 1-1 binding to LibSass. See [LibSass wiki](https://github.com/sass/libsass/wiki "LibSass wiki") for documentation.

### Static, shared or external version of Libsass?

By default, hLibsass uses a local (built during `cabal configure`), static version of libsass. This is the recommended approach, as it ensures that the package uses compatible version of the library. However, this implies that the `libsass.a` file will be copied to the installation directory of the package (probably to a sandbox, so this should not be a problem) and GHCi won't work neither for this package nor any other that depends on hLibsass (it will either segfault or complain about unresolved symbols).

hLibsass may be configured to use a shared version of LibSass. Just specify the `sharedLibsass` flag during `configure`/`install` and hLibsass will build the shared version. This will build a `.so` (or `.a` + `.dll`) file that will be copied to the installation directory and allow GHCi to work properly (except on Windows - but that's because of an import library that hLibsass needs to link against). This may require adjusting `LD_LIBRARY_PATH` (it *should* work without this, but I was unable to make it work all the time). You may use [tools/libpath.sh](tools/libpath.sh) to extract correct path from `ghc-pkg`.

hLibsass may use LibSass version installed in the system - specify `externalLibsass` flag and the build process will not build local version of LibSass and just relay on the existing one.

### Things to consider

Libsass is C++ library with C API, so in order to use it, it is necessary to provide C++ runtime. This library is linked against `libstdc++` automatically, so you don't have to deal with linking process.

### Copyright

Copyright (c) 2015 Jakub Fijałkowski. See LICENSE for details.
