{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Sass.Wrappers where

import Foreign
import Binding.Sass.Types

foreign import ccall safe "wrapper" mkSassImporterFn
    :: SassImporterFnType
    -> IO (SassImporterFn)

foreign import ccall safe "wrapper" mkSassFunctionFn
    :: SassFunctionFnType
    -> IO (SassFunctionFn)