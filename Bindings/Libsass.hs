-- |
-- This is the main module of the library - it reexports rest of the library, so
-- you should only import this one.

{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libsass
  (
    libsass_version
  , module Libsass
  ) where

import           Bindings.Libsass.Context   as Libsass
import           Bindings.Libsass.Functions as Libsass
import           Bindings.Libsass.Types     as Libsass
import           Bindings.Libsass.Wrappers  as Libsass
import           Bindings.Libsass.Values    as Libsass
import           Foreign.C.String

foreign import ccall unsafe "sass/base.h" libsass_version :: CString
