{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Libsass.Wrappers where

import Foreign
import Binding.Libsass.Types

foreign import ccall safe "wrapper" mkSassImporterFn
    :: SassImporterFnType
    -> IO (SassImporterFn)

foreign import ccall safe "wrapper" mkSassFunctionFn
    :: SassFunctionFnType
    -> IO (SassFunctionFn)