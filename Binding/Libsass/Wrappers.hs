{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Libsass.Wrappers where

import           Binding.Libsass.Types
import           Foreign

foreign import ccall safe "wrapper" mkSassImporterFn
    :: SassImporterFnType
    -> IO SassImporterFn

foreign import ccall safe "wrapper" mkSassFunctionFn
    :: SassFunctionFnType
    -> IO SassFunctionFn

foreign import ccall "sass_context.h &sass_delete_compiler"
    p_sass_delete_compiler :: FunPtr (Ptr SassCompiler -> IO ())

foreign import ccall "sass_context.h &sass_delete_file_context"
    p_sass_delete_file_context :: FunPtr (Ptr SassFileContext -> IO ())

foreign import ccall "sass_context.h &sass_delete_data_context"
    p_sass_delete_data_context :: FunPtr (Ptr SassDataContext -> IO ())

foreign import ccall "sass_function.h &sass_delete_import_list"
    p_sass_delete_import_list :: FunPtr (SassImportList -> IO ())

foreign import ccall "sass_function.h &sass_delete_import"
    p_sass_delete_import :: FunPtr (SassImportEntry -> IO ())

foreign import ccall "sass_function.h &sass_delete_importer"
    p_sass_delete_importer :: FunPtr (SassImporterEntry -> IO ())

foreign import ccall  "sass_values.h &sass_delete_value"
    p_sass_delete_value :: FunPtr (Ptr SassValue -> IO ())
