hLibsass
========

hLibsass is a low-level binding to [Libsass](https://github.com/sass/libsass "Libsass"). It provides basic types used by library along with FFI interface to the C API.

### Documentation

This library is (mostly) 1-1 binding to Libsass. See [Libsass wiki](https://github.com/sass/libsass/wiki "Libsas wiki") for documentation.

### Considerations

This library compiles libsass as a static library in configuration process. When you `cabal install` the library, it copies `libsass.a` to the destination directory along the Haskell library, so you should avoid installing it globally.

Libsass is C++ library with C API, so in order to use it, it is necessary to provide C++ runtime. This library is linked against `libstdc++` automatically, so you don't have to deal with linking process.

### Copyright

Copyright (c) 2015 Jakub Fijałkowski. See LICENSE for details.
