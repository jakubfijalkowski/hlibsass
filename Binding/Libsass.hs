{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Libsass
  (
    libsass_version
  , module Libsass
  ) where

import           Binding.Libsass.Context  as Libsass
import           Binding.Libsass.Types    as Libsass
import           Binding.Libsass.Wrappers as Libsass
import           Foreign.C.String

foreign import ccall unsafe "sass.h" libsass_version :: CString
