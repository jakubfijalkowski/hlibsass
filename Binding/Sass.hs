{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Sass
  (
    libsass_version
  , module Sass
  ) where

import           Binding.Sass.Context  as Sass
import           Binding.Sass.Types    as Sass
import           Binding.Sass.Wrappers as Sass
import           Foreign.C.String

foreign import ccall unsafe "sass.h" libsass_version :: CString
