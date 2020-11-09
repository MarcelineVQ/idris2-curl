module Main

import Data.IORef

import Data.Buffer
import Network.Curl.Easy

writeFunction : Buffer -> (len : Int) -> IORef (List Buffer) -> PrimIO Int
writeFunction buf len ref = toPrim $ do
    lst <- readIORef ref
    writeIORef ref $ lst ++ [buf]
    rawSize buf

main : IO ()
main = do
    CURLE_OK <- curl_global_init | c => do
        putStrLn $ "error in curl_global_init: " ++ show c

    Just cli <- curlEasyInit | Nothing => putStrLn "error in curlEasyInit"

    CURLE_OK <- curlEasySetopt cli CURLOPT_URL "https://www.google.com/" | c => do
        putStrLn $ "error in curlEasySetopt: " ++ show c

    ref <- newIORef []
    CURLE_OK <- curlEasySetopt cli CURLOPT_WRITEFUNCTION
        (\b,_,l,_ => writeFunction b l ref) | c => do
          putStrLn $ "error in curlEasySetopt: " ++ show c

    CURLE_OK <- curlEasyPerform cli | c => do
        putStrLn $ "error in curlEasyPerform: " ++ show c

    buffers <- readIORef ref
    putStrLn $ "rawSize buffers = " ++ show (length buffers)
    Just buffer <- concatBuffers buffers | Nothing => putStrLn "concatBuffers returned Nothing"
    bufferSize <- rawSize buffer
    body <- getString buffer 0 bufferSize
    putStrLn body
    -- traverse freeBuffer buffers
    -- freeBuffer buffer

    curlEasyCleanup cli
    curl_global_cleanup
