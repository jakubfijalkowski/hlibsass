{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
import           Bindings.Libsass
import           Foreign
import           Foreign.C
import           Test.Hspec

simpleCompile :: String -> IO String
simpleCompile str = do
    cstr <- newCString str
    ctx <- sass_make_data_context cstr
    status <- sass_compile_data_context ctx
    if status /= 0
        then do
            sass_delete_data_context ctx
            return ""
        else do
            cres <- sass_context_get_output_string (castPtr ctx)
            !res <- peekCString cres
            sass_delete_data_context ctx
            return res

sampleInput :: String
sampleInput = "foo { margin: 21px * 2; }"

sampleOutput :: String
sampleOutput = "foo {\n  margin: 42px; }\n"

main :: IO ()
main = hspec $
  describe "Libsass" $ do
    it "should correctly compile simple expression" $
        simpleCompile sampleInput `shouldReturn` sampleOutput

#ifndef EXTERNAL_LIBSASS
    it "should report correct version" $ do
        str <- peekCString libsass_version
        str `shouldBe` "3.6.3"
#endif

    it "should support quoted strings" $ withCString "sample" $ \cstr -> do
        str <- sass_make_qstring cstr
        sass_string_is_quoted str `shouldReturn` True

    it "should correctly combine two SassValues" $ withCString "" $ \unit -> do
        val1 <- sass_make_number 1.0 unit
        val2 <- sass_make_number 2.0 unit
        val3 <- sass_value_op (fromIntegral $ fromEnum SassAdd) val1 val2
        result <- sass_number_get_value val3
        sass_delete_value val1
        sass_delete_value val2
        sass_delete_value val3
        result `shouldBe` 3.0
