module Main

import Data.Buffer
import Network.Curl.Easy

-- %foreign "C:alter_ptr,libhelper"
ffi__alter_ptr : (t -> PrimIO t) -> Ptr t -> PrimIO ()
-- %foreign ...
ffi__to_ptr : t -> PrimIO (Ptr t)
-- %foreign ...
ffi__from_ptr : Ptr t -> PrimIO t

partial
expect : String -> (1 _ : Maybe t) -> t
expect msg Nothing = idris_crash msg
expect msg (Just val) = val

partial
writeFunction : Buffer -> (itemsize : Int) -> (len : Int) -> AnyPtr -> PrimIO Int
writeFunction buf itemsize len anyptr = toPrim $ writeFunction' buf len (prim__castPtr anyptr) where
    partial
    passToAlter : Buffer -> Buffer -> IO Buffer
    passToAlter newbuf storage = do
        newStorage <- concatBuffers [storage, newbuf]
        pure $ expect "concatBuffers returned Nothing" newStorage
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

    bufferOpt <- newBuffer 0
    let buffer = expect "newBuffer returned Nothing" bufferOpt
    ptrBuffer <- primIO $ ffi__to_ptr buffer
    CURLE_OK <- curlEasySetopt cli CURLOPT_WRITEDATA (prim__forgetPtr ptrBuffer)
    CURLE_OK <- curlEasySetopt cli CURLOPT_WRITEFUNCTION writeFunction | c => do
        putStrLn $ "error in curlEasySetopt: " ++ show c

    CURLE_OK <- curlEasyPerform cli | c => do
        putStrLn $ "error in curlEasyPerform: " ++ show c

    buffer <- primIO $ ffi__from_ptr ptrBuffer
    bufferSize <- rawSize buffer
    body <- getString buffer 0 bufferSize
    freeBuffer buffer

    curlEasyCleanup cli
    curl_global_cleanup
