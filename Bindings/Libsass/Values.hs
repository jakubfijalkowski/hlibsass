{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.Libsass.Values where

import           Bindings.Libsass.Types
import           Foreign
import           Foreign.C

foreign import ccall unsafe "sass_values.h" sass_value_get_tag
    :: Ptr SassValue
    -> IO CInt -- ^ Returns 'SassTag'

foreign import ccall unsafe "sass_values.h" sass_value_is_null
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_number
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_string
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_boolean
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_color
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_list
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_map
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_error
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_value_is_warning
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_number_get_value
    :: Ptr SassValue
    -> IO CDouble

foreign import ccall unsafe "sass_values.h" sass_number_set_value
    :: Ptr SassValue
    -> CDouble
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_number_get_unit
    :: Ptr SassValue
    -> IO CString

foreign import ccall unsafe "sass_values.h" sass_number_set_unit
    :: Ptr SassValue
    -> CString
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_string_get_value
    :: Ptr SassValue
    -> IO CString

foreign import ccall unsafe "sass_values.h" sass_string_is_quoted
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_string_set_quoted
    :: Ptr SassValue
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_string_set_value
    :: Ptr SassValue
    -> CString
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_boolean_get_value
    :: Ptr SassValue
    -> IO Bool

foreign import ccall unsafe "sass_values.h" sass_boolean_set_value
    :: Ptr SassValue
    -> Bool
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_color_get_r
    :: Ptr SassValue
    -> IO CDouble

foreign import ccall unsafe "sass_values.h" sass_color_set_r
    :: Ptr SassValue
    -> CDouble
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_color_get_g
    :: Ptr SassValue
    -> IO CDouble

foreign import ccall unsafe "sass_values.h" sass_color_set_g
    :: Ptr SassValue
    -> CDouble
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_color_get_b
    :: Ptr SassValue
    -> IO CDouble

foreign import ccall unsafe "sass_values.h" sass_color_set_b
    :: Ptr SassValue
    -> CDouble
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_color_get_a
    :: Ptr SassValue
    -> IO CDouble

foreign import ccall unsafe "sass_values.h" sass_color_set_a
    :: Ptr SassValue
    -> CDouble
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_list_get_length
    :: Ptr SassValue
    -> IO CSize

foreign import ccall unsafe "sass_values.h" sass_list_get_separator
    :: Ptr SassValue
    -> IO CInt -- ^ Retrurns 'SassSeparator'

foreign import ccall unsafe "sass_values.h" sass_list_set_separator
    :: Ptr SassValue
    -> CInt -- ^ 'SassSeparator'
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_list_get_value
    :: Ptr SassValue
    -> CSize
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_list_set_value
    :: Ptr SassValue
    -> CSize
    -> Ptr SassValue
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_map_get_length
    :: Ptr SassValue
    -> IO CSize

foreign import ccall unsafe "sass_values.h" sass_map_get_key
    :: Ptr SassValue
    -> CSize
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_map_set_key
    :: Ptr SassValue
    -> CSize
    -> Ptr SassValue
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_map_get_value
    :: Ptr SassValue
    -> CSize
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_map_set_value
    :: Ptr SassValue
    -> CSize
    -> Ptr SassValue
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_error_get_message
    :: Ptr SassValue
    -> IO CString

foreign import ccall unsafe "sass_values.h" sass_error_set_message
    :: Ptr SassValue
    -> CString
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_warning_get_message
    :: Ptr SassValue
    -> IO CString

foreign import ccall unsafe "sass_values.h" sass_warning_set_message
    :: Ptr SassValue
    -> CString
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_make_null
    :: IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_boolean
    :: Bool
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_string
    :: CString
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_qstring
    :: CString
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_number
    :: CDouble
    -> CString
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_color
    :: CDouble
    -> CDouble
    -> CDouble
    -> CDouble
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_list
    :: CSize
    -> CInt -- ^ 'SassSeparator'
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_map
    :: CSize
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_error
    :: CString
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_make_warning
    :: CString
    -> IO (Ptr SassValue)

foreign import ccall unsafe "sass_values.h" sass_delete_value
    :: Ptr SassValue
    -> IO ()

foreign import ccall unsafe "sass_values.h" sass_clone_value
    :: Ptr SassValue
    -> IO (Ptr SassValue)
