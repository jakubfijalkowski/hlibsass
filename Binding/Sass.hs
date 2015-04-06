{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Sass(
    libsass_version,
    module Binding.Sass.Context) where

import           Binding.Sass.Context ()
import           Foreign.C.String

foreign import ccall unsafe "sass.h" libsass_version :: CString
