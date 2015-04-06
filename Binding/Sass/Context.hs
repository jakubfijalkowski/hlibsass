{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Binding.Sass.Context where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data SassCompiler
data SassOptions
data SassContext
data SassFileContext
data SassDataContext

data SassCompilerState = SassCompilerCreated
                       | SassCompilerParsed
                       | SassCompilerExecuted
                       deriving (Show, Eq)

instance Enum SassCompilerState where
    fromEnum SassCompilerCreated  = 0
    fromEnum SassCompilerParsed   = 1
    fromEnum SassCompilerExecuted = 2

    toEnum 0 = SassCompilerCreated
    toEnum 1 = SassCompilerParsed
    toEnum 2 = SassCompilerExecuted

foreign import ccall safe "sass_context.h" sass_make_options
    :: IO (Ptr SassOptions)

foreign import ccall safe "sass_context.h" sass_make_file_context
    :: CString
    -> IO (Ptr SassFileContext)

foreign import ccall safe "sass_context.h" sass_make_data_context
    :: CString
    -> IO (Ptr SassDataContext)

foreign import ccall safe "sass_context.h" sass_compile_file_context
    :: Ptr SassFileContext
    -> IO CInt

foreign import ccall safe "sass_context.h" sass_compile_data_context
    :: Ptr SassDataContext
    -> IO CInt

foreign import ccall safe "sass_context.h" sass_make_file_compiler
    :: Ptr SassFileContext
    -> IO (Ptr SassCompiler)

foreign import ccall safe "sass_context.h" sass_make_data_compiler
    :: Ptr SassDataContext
    -> IO (Ptr SassCompiler)

foreign import ccall safe "sass_context.h" sass_compiler_parse
    :: Ptr SassCompiler
    -> IO CInt

foreign import ccall safe "sass_context.h" sass_compiler_execute
    :: Ptr SassCompiler
    -> IO CInt

foreign import ccall safe "sass_context.h" sass_delete_compiler
    :: Ptr SassCompiler
    -> IO ()

foreign import ccall safe "sass_context.h" sass_delete_file_context
    :: Ptr SassFileContext
    -> IO ()

foreign import ccall safe "sass_context.h" sass_delete_data_context
    :: Ptr SassDataContext
    -> IO ()

foreign import ccall safe "sass_context.h" sass_file_context_get_context
    :: Ptr SassFileContext
    -> Ptr SassContext

foreign import ccall safe "sass_context.h" sass_data_context_get_context
    :: Ptr SassDataContext
    -> Ptr SassContext

foreign import ccall safe "sass_context.h" sass_context_get_options
    :: Ptr SassContext
    -> Ptr SassOptions

foreign import ccall safe "sass_context.h" sass_file_context_get_options
    :: Ptr SassFileContext
    -> Ptr SassOptions

foreign import ccall safe "sass_context.h" sass_data_context_get_options
    :: Ptr SassDataContext
    -> Ptr SassOptions

foreign import ccall safe "sass_context.h" sass_file_context_set_options
    :: Ptr SassFileContext
    -> Ptr SassOptions
    -> IO ()

foreign import ccall safe "sass_context.h" sass_data_context_set_options
    :: Ptr SassDataContext
    -> Ptr SassOptions
    -> IO ()

foreign import ccall safe "sass_context.h" sass_context_get_output_string
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_error_status
    :: Ptr SassContext
    -> IO CInt

foreign import ccall safe "sass_context.h" sass_context_get_error_json
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_error_text
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_error_message
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_error_file
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_error_src
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_error_line
    :: Ptr SassContext
    -> IO CSize

foreign import ccall safe "sass_context.h" sass_context_get_error_column
    :: Ptr SassContext
    -> IO CSize

foreign import ccall safe "sass_context.h" sass_context_get_source_map_string
    :: Ptr SassContext
    -> IO CString

foreign import ccall safe "sass_context.h" sass_context_get_included_files
    :: Ptr SassContext
    -> IO (Ptr CString)