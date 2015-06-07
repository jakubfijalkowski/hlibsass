hLibsass
========

hLibsass is a low-level binding to [Libsass](https://github.com/sass/libsass "Libsass"). It provides basic types used by library along with FFI interface to the C API.

It is available on [Hackage](http://hackage.haskell.org/package/hlibsass).

### Documentation

This library is (mostly) 1-1 binding to Libsass. See [Libsass wiki](https://github.com/sass/libsass/wiki "Libsas wiki") for documentation.

### Static, shared or external version of Libsass?

By default, hLibsass uses a local (built during `cabal configure`), static version of libsass. This is the recommended approach, as it ensures that the package uses compatible version of the library. However, this implies that the `libsass.a` file will be copied to the installation directory of the package (probably to a sandbox, so this should not be a problem) and GHCi won't work neither for this package nor any other that depends on hLibsass (it will either segfault or complain about unresolved symbols).

hLibsass may be configured to use a shared version of libsass. Just specify the `sharedLibsass` flag during `configure`/`install` and hLibsass will build the shared version. This will build a `.so` (or `.a` + `.dll`) file that will be copied to the installation directory and allow GHCi to work properly (except on Windows - but that's because of an import library that hLibsass needs to link against). This may require adjusting `LD_LIBRARY_PATH` (it *should* work without this, but I was unable to make it work all the time). You may use [tools/libpath.sh](tools/libpath.sh) to extract correct path from `ghc-pkg`.

hLibsass may use libsass version installed in the system - specify `externalLibsass` flag and the build process will not build local version of libsass and just relay on the existing one.

### Things to consider

Libsass is C++ library with C API, so in order to use it, it is necessary to provide C++ runtime. This library is linked against `libstdc++` automatically, so you don't have to deal with linking process.

### Copyright

Copyright (c) 2015 Jakub Fijałkowski. See LICENSE for details.
