module Main

import Data.Buffer
import Network.Curl.Easy

-- %foreign "C:alter_ptr,libhelper"
ffi__alter_ptr : (t -> PrimIO t) -> Ptr t -> PrimIO ()

partial
writeFunction : Buffer -> (itemsize : Int) -> (len : Int) -> AnyPtr -> PrimIO Int
writeFunction buf itemsize len anyptr = toPrim $ writeFunction' buf len (prim__castPtr anyptr) where
    partial
    passToAlter : Buffer -> Buffer -> IO Buffer
    passToAlter newbuf storage = do
        newStorage <- concatBuffers [storage, newbuf]
        case newStorage of
            Nothing => idris_crash "concatBuffers returned Nothing"
            Just x => pure x
    partial
    writeFunction' : Buffer -> (len : Int) -> Ptr Buffer -> IO Int
    writeFunction' newbuf len ptr = do
        let callback = passToAlter newbuf
        let primCallback = toPrim . callback
        primIO $ ffi__alter_ptr primCallback ptr
        pure len

partial
main : IO ()
main = do
    CURLE_OK <- curl_global_init | c => do
        putStrLn $ "error in curl_global_init: " ++ show c

    Just cli <- curlEasyInit | Nothing => putStrLn "error in curlEasyInit"

    CURLE_OK <- curlEasySetopt cli CURLOPT_URL "https://google.com" | c => do
        putStrLn $ "error in curlEasySetopt: " ++ show c

    CURLE_OK <- curlEasySetopt cli CURLOPT_WRITEFUNCTION writeFunction | c => do
        putStrLn $ "error in curlEasySetopt: " ++ show c

    CURLE_OK <- curlEasyPerform cli | c => do
        putStrLn $ "error in curlEasyPerform: " ++ show c

    curlEasyCleanup cli
    curl_global_cleanup
