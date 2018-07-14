hLibsass
========
[![Build Status](https://travis-ci.org/jakubfijalkowski/hlibsass.svg?branch=master)](https://travis-ci.org/jakubfijalkowski/hlibsass)
[![Hackage](https://img.shields.io/hackage/v/hlibsass.svg)](http://hackage.haskell.org/package/hlibsass)
[![Hackage deps](https://img.shields.io/hackage-deps/v/hlibsass.svg)](http://packdeps.haskellers.com/reverse/hlibsass)

hLibsass is a low-level binding to [LibSass](https://github.com/sass/libsass "LibSass"). It provides basic types used by the library along with a FFI interface to the C API.

This is really a low-level library, if you want a high-level, more Haskell-friendly library, check out [hSass](https://github.com/jakubfijalkowski/hsass).

It is available on [Hackage](http://hackage.haskell.org/package/hlibsass).

[hLibsass' Changelog](CHANGELOG.md)

### Documentation

This library is (mostly) 1-1 binding to LibSass. See [LibSass wiki](https://github.com/sass/libsass/wiki "LibSass wiki") for documentation.

### Static, shared or external version of LibSass?

hLibsass supports three different ways of linking with Libsass:

1. Static, locally-built LibSass,
2. Shared, locally-built LibSass, requires setting `sharedLibsass` flag,
3. External (e.g. system package), requires setting `externalLibsass` flag.

You can pick your preferred option by editing your projects `stack.yaml` file thus:

``` 
# THIS SHOULD BE IN YOUR stack.yaml FILE

flags:
  hlibsass:
    sharedlibsass: false
    externallibsass: true
```

**What do these configuration flags mean?**

| sharedlibsas | externallibsass | Behaviour |
| --- | --- | --- |
| false | false | Static, locally built libsass will be used |
| true | false | Shared, locally built libsass will be used |
| N/A | true | External libSass provided by your system's package manager will be used. If `externallibsass=true` then the `sharedlibsass` flag is ignored |

**Caveats for using "Static, locally built libsass"**

TODO

**Caveats for using "Shared, locally built libsass"**

TODO

**Caveats for using "External libsass"**

TODO


The first two options rely on the LibSass being distributed with the `hlibsass` package and compiled during configure/build process. hLibsass is always distributed with LibSass version that is compatibile with the package.

Up until version `0.1.6.0`, the first option was preferred, because LibSass has been distributed as a source package only. Development package of LibSass is now available in most Linux distributions and in Homebrew, so using it is equally good option

Using locally-built **shared** version of LibSass may require adjusting `LD_LIBRARY_PATH` or putting `libsass.so` somewhere in the `PATH`, so this option is discouraged. However, if you really want to use it that way, you may use [tools/libpath.sh](tools/libpath.sh) to set `LD_LIBRARY_PATH` (it extracts the library location using `ghc-pkg`).

`externalLibsass` flag has precedence over `sharedLibsass`.

### hLibsass and LibSass version

hLibsass uses LibSass internally and therefore is quite tightly bound to LibSass. Every breaking change in LibSass C API will break hLibSass (either at compile-time or at runtime, depends on the change). For now, these combinations should work:

- hLibsass 0.1.5.x with LibSass 3.3.2 (distributed with) and 3.4.3 (not every function is reexported, but there were no changes that would break hLibSass),
- hLibsass 0.1.6.x with LibSass 3.4.3 (distributed with),
- hLibsass 0.1.7.x with LibSass 3.5.2 (distributed with)
- LibSass 3.5.x **WILL** have breaking changes and will result in runtime failures of hLibSass versions prior to 0.1.7.0 (and probably compile-time failures too), see [LibSass releases page](https://github.com/sass/libsass/releases) for more details.

### Things to consider

Libsass is C++ library with C API, so in order to use it, it is necessary to provide C++ runtime. This library is linked against `libstdc++` (Windows & Linux)/`libc++` (macOS) automatically, so you don't have to deal with linking process.

### Known limitations

#### GHCi not able to load hLibSass

GHC 7 is unable to load static version of native libraries (at least some of them) and therefore it rejects loading `libsass.a`. On Linux, you can overcome this by switch to GHC 8 (tested with 8.0.2 with LTS Haskell 8.5) or by using shared or external (also shared) version of LibSass. On macOS, GHC 8 still rejects the library (it detects unresolver/duplicated symbols), so the only solution is to use shared/external LibSass (Homebrew version works).

### Copyright

Copyright (c) 2015-2018 Jakub Fijałkowski. See [LICENSE](LICENSE) for details.
