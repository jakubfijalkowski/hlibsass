{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libsass.Functions where

import           Bindings.Libsass.Types
import           Foreign
import           Foreign.C

foreign import ccall unsafe "sass/functions.h" sass_make_importer_list
    :: CSize
    -> IO SassImporterList

foreign import ccall unsafe "sass/functions.h" sass_importer_get_list_entry
    :: SassImporterList
    -> CSize
    -> IO SassImporterEntry

foreign import ccall unsafe "sass/functions.h" sass_importer_set_list_entry
    :: SassImporterList
    -> CSize
    -> SassImporterEntry
    -> IO ()

foreign import ccall unsafe "sass/functions.h" sass_make_importer
    :: SassImporterFn
    -> CDouble
    -> Ptr ()
    -> IO SassImporterEntry

foreign import ccall unsafe "sass/functions.h" sass_importer_get_function
    :: SassImporterEntry
    -> IO SassImporterFn

foreign import ccall unsafe "sass/functions.h" sass_importer_get_priority
    :: SassImporterEntry
    -> IO CDouble
foreign import ccall unsafe "sass/functions.h" sass_importer_get_cookie
    :: SassImporterEntry
    -> IO (Ptr ())

foreign import ccall unsafe "sass/functions.h" sass_delete_importer
    :: SassImporterEntry
    -> IO ()

foreign import ccall unsafe "sass/functions.h" sass_make_import_list
    :: CSize
    -> IO SassImportList

foreign import ccall unsafe "sass/functions.h" sass_make_import_entry
    :: CString
    -> CString
    -> CString
    -> IO SassImportEntry

foreign import ccall unsafe "sass/functions.h" sass_make_import
    :: CString
    -> CString
    -> CString
    -> CString
    -> IO SassImportEntry

foreign import ccall unsafe "sass/functions.h" sass_import_set_error
    :: SassImportEntry
    -> CString
    -> CSize
    -> CSize
    -> IO SassImportEntry

foreign import ccall unsafe "sass/functions.h" sass_import_set_list_entry
    :: SassImportList
    -> CSize
    -> SassImportEntry
    -> IO ()

foreign import ccall unsafe "sass/functions.h" sass_import_get_list_entry
    :: SassImportList
    -> CSize
    -> IO SassImportEntry

foreign import ccall unsafe "sass/functions.h" sass_import_imp_path
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_import_get_abs_path
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_import_get_source
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_import_get_srcmap
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_import_take_source
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_import_take_srcmap
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_import_get_error_line
    :: SassImportEntry
    -> IO CSize

foreign import ccall unsafe "sass/functions.h" sass_import_get_error_column
    :: SassImportEntry
    -> IO CSize

foreign import ccall unsafe "sass/functions.h" sass_import_get_error_message
    :: SassImportEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_delete_import_list
    :: SassImportList
    -> IO ()

foreign import ccall unsafe "sass/functions.h" sass_delete_import
    :: SassImportEntry
    -> IO ()

foreign import ccall unsafe "sass/functions.h" sass_make_function_list
    :: CSize
    -> IO SassFunctionList

foreign import ccall unsafe "sass/functions.h" sass_make_function
    :: CString
    -> SassFunctionFn
    -> Ptr ()
    -> IO SassFunctionEntry

foreign import ccall unsafe "sass/functions.h" sass_function_get_list_entry
    :: SassFunctionList
    -> CSize
    -> IO SassFunctionEntry

foreign import ccall unsafe "sass/functions.h" sass_function_set_list_entry
    :: SassFunctionList
    -> CSize
    -> SassFunctionEntry
    -> IO ()

foreign import ccall unsafe "sass/functions.h" sass_function_get_signature
    :: SassFunctionEntry
    -> IO CString

foreign import ccall unsafe "sass/functions.h" sass_function_get_function
    :: SassFunctionEntry
    -> IO SassFunctionFn

foreign import ccall unsafe "sass/functions.h" sass_function_get_cookie
    :: SassFunctionEntry
    -> IO (Ptr ())
