{-# LANGUAGE BangPatterns #-}
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
main = hspec $ do
  describe "Libsass" $ do
    it "should correctly compile simple expression" $ do
      simpleCompile sampleInput `shouldReturn` sampleOutput
