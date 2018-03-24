{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libsass.Context where

import           Bindings.Libsass.Types
import           Foreign
import           Foreign.C

foreign import ccall unsafe "sass/context.h" sass_make_options
    :: IO (Ptr SassOptions)

foreign import ccall unsafe "sass/context.h" sass_make_file_context
    :: CString
    -> IO (Ptr SassFileContext)

-- | Creates and initializes a data context, ie. context that parses string
--   instead of a file.
--
--   WARNING! The string that this function takes is released by the libsass
--   during cleanup. You must not deallocate it.
foreign import ccall unsafe "sass/context.h" sass_make_data_context
    :: CString
    -> IO (Ptr SassDataContext)

foreign import ccall safe "sass/context.h" sass_compile_file_context
    :: Ptr SassFileContext
    -> IO CInt

foreign import ccall safe "sass/context.h" sass_compile_data_context
    :: Ptr SassDataContext
    -> IO CInt

foreign import ccall unsafe "sass/context.h" sass_make_file_compiler
    :: Ptr SassFileContext
    -> IO (Ptr SassCompiler)

foreign import ccall unsafe "sass/context.h" sass_make_data_compiler
    :: Ptr SassDataContext
    -> IO (Ptr SassCompiler)

foreign import ccall safe "sass/context.h" sass_compiler_parse
    :: Ptr SassCompiler
    -> IO CInt

foreign import ccall safe "sass/context.h" sass_compiler_execute
    :: Ptr SassCompiler
    -> IO CInt

foreign import ccall unsafe "sass/context.h" sass_delete_compiler
    :: Ptr SassCompiler
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_delete_options
    :: Ptr SassOptions
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_delete_file_context
    :: Ptr SassFileContext
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_delete_data_context
    :: Ptr SassDataContext
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_file_context_get_context
    :: Ptr SassFileContext
    -> Ptr SassContext

foreign import ccall unsafe "sass/context.h" sass_data_context_get_context
    :: Ptr SassDataContext
    -> Ptr SassContext

foreign import ccall unsafe "sass/context.h" sass_context_get_options
    :: Ptr SassContext
    -> Ptr SassOptions

foreign import ccall unsafe "sass/context.h" sass_file_context_get_options
    :: Ptr SassFileContext
    -> Ptr SassOptions

foreign import ccall unsafe "sass/context.h" sass_data_context_get_options
    :: Ptr SassDataContext
    -> Ptr SassOptions

foreign import ccall unsafe "sass/context.h" sass_file_context_set_options
    :: Ptr SassFileContext
    -> Ptr SassOptions
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_data_context_set_options
    :: Ptr SassDataContext
    -> Ptr SassOptions
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_get_precision
    :: Ptr SassOptions
    -> IO CInt

foreign import ccall unsafe "sass/context.h" sass_option_get_output_style
    :: Ptr SassOptions
    -> IO CInt -- ^ Returns 'SassOutputStyle'

foreign import ccall unsafe "sass/context.h" sass_option_get_source_comments
    :: Ptr SassOptions
    -> IO Bool

foreign import ccall unsafe "sass/context.h" sass_option_get_source_map_embed
    :: Ptr SassOptions
    -> IO Bool

foreign import ccall unsafe "sass/context.h" sass_option_get_source_map_contents
    :: Ptr SassOptions
    -> IO Bool

foreign import ccall unsafe "sass/context.h" sass_option_get_source_map_file_urls
    :: Ptr SassOptions
    -> IO Bool

foreign import ccall unsafe "sass/context.h" sass_option_get_omit_source_map_url
    :: Ptr SassOptions
    -> IO Bool

foreign import ccall unsafe "sass/context.h" sass_option_get_is_indented_syntax_src
    :: Ptr SassOptions
    -> IO Bool

foreign import ccall unsafe "sass/context.h" sass_option_get_indent
    :: Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_option_get_linefeed
    :: Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_option_get_input_path
    :: Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_option_get_output_path
    :: Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_option_get_source_map_file
    :: Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_option_get_source_map_root
    :: Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_option_get_c_headers
     :: Ptr SassOptions
     -> IO SassImporterList

foreign import ccall unsafe "sass/context.h" sass_option_get_c_importers
     :: Ptr SassOptions
     -> IO SassImporterList

foreign import ccall unsafe "sass/context.h" sass_option_get_c_functions
     :: Ptr SassOptions
     -> IO SassFunctionList

foreign import ccall unsafe "sass/context.h" sass_option_set_precision
    :: Ptr SassOptions
    -> CInt
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_output_style
    :: Ptr SassOptions
    -> CInt -- ^ 'SassOutputStyle'
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_source_comments
    :: Ptr SassOptions
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_source_map_embed
    :: Ptr SassOptions
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_source_map_contents
    :: Ptr SassOptions
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_source_map_file_urls
    :: Ptr SassOptions
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_omit_source_map_url
    :: Ptr SassOptions
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_is_indented_syntax_src
    :: Ptr SassOptions
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_indent
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_linefeed
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_input_path
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_output_path
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_plugin_path
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_include_path
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_source_map_file
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_source_map_root
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_c_headers
    :: Ptr SassOptions
    -> SassImporterList
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_c_importers
    :: Ptr SassOptions
    -> SassImporterList
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_set_c_functions
    :: Ptr SassOptions
    -> SassFunctionList
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_context_get_output_string
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_error_status
    :: Ptr SassContext
    -> IO CInt

foreign import ccall unsafe "sass/context.h" sass_context_get_error_json
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_error_text
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_error_message
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_error_file
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_error_src
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_error_line
    :: Ptr SassContext
    -> IO CSize

foreign import ccall unsafe "sass/context.h" sass_context_get_error_column
    :: Ptr SassContext
    -> IO CSize

foreign import ccall unsafe "sass/context.h" sass_context_get_source_map_string
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_included_files
    :: Ptr SassContext
    -> IO (Ptr CString)

foreign import ccall unsafe "sass/context.h" sass_option_get_include_path_size
    :: Ptr SassOptions
    -> IO CSize

foreign import ccall unsafe "sass/context.h" sass_option_get_include_path
    :: Ptr SassOptions
    -> CSize
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_get_included_files_size
    :: Ptr SassContext
    -> IO CSize

foreign import ccall unsafe "sass/context.h" sass_context_take_error_json
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_take_error_text
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_take_error_message
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_take_error_file
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_take_output_string
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_take_source_map_string
    :: Ptr SassContext
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_context_take_included_files
    :: Ptr SassContext
    -> IO (Ptr CString)

foreign import ccall unsafe "sass/context.h" sass_compiler_get_state
    :: Ptr SassCompiler
    -> IO CInt -- ^ Retruns 'SassCompilerState'

foreign import ccall unsafe "sass/context.h" sass_compiler_get_context
    :: Ptr SassCompiler
    -> IO (Ptr SassContext)

foreign import ccall unsafe "sass/context.h" sass_compiler_get_options
    :: Ptr SassCompiler
    -> IO (Ptr SassOptions)

foreign import ccall unsafe "sass/context.h" sass_compiler_get_import_stack_size
    :: Ptr SassCompiler
    -> IO CSize

foreign import ccall unsafe "sass/context.h" sass_compiler_get_last_import
    :: Ptr SassCompiler
    -> IO SassImportEntry

foreign import ccall unsafe "sass/context.h" sass_compiler_get_import_entry
    :: Ptr SassCompiler
    -> CSize
    -> IO SassImportEntry

foreign import ccall unsafe "sass/context.h" sass_compiler_get_callee_stack_size
    :: Ptr SassCompiler
    -> IO CSize

foreign import ccall unsafe "sass/context.h" sass_compiler_get_last_callee
    :: Ptr SassCompiler
    -> IO SassCalleeEntry

foreign import ccall unsafe "sass/context.h" sass_compiler_get_callee_entry
    :: Ptr SassCompiler
    -> CSize
    -> IO SassCalleeEntry

foreign import ccall unsafe "sass/context.h" sass_option_push_plugin_path
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_option_push_include_path
    :: Ptr SassOptions
    -> CString
    -> IO ()

foreign import ccall unsafe "sass/context.h" sass_find_file
    :: CString
    -> Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_find_include
    :: CString
    -> Ptr SassOptions
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_compiler_find_file
    :: CString
    -> Ptr SassCompiler
    -> IO CString

foreign import ccall unsafe "sass/context.h" sass_compiler_find_include
    :: CString
    -> Ptr SassCompiler
    -> IO CString
    
