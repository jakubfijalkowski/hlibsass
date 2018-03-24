{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libsass.Base where

import           Foreign
import           Foreign.C

foreign import ccall unsafe "sass/base.h" sass_alloc_memory
    :: CSize
    -> IO (Ptr ())

foreign import ccall unsafe "sass/base.h" sass_copy_c_string
    :: CString
    -> IO CString

foreign import ccall unsafe "sass/base.h" sass_free_memory
    :: Ptr ()
    -> IO ()

foreign import ccall unsafe "sass/base.h" sass_string_quote
    :: CString
    -> CChar
    -> IO CString

foreign import ccall unsafe "sass/base.h" sass_string_unquote
    :: CString
    -> IO CString

foreign import ccall unsafe "sass/base.h" libsass_version
    :: CString

foreign import ccall unsafe "sass/base.h" libsass_language_version
    :: CString
