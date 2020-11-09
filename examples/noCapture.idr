module Main

import Network.Curl.Easy

main : IO ()
main = do
    CURLE_OK <- curl_global_init | c => do
        putStrLn $ "error in curl_global_init: " ++ show c

    Just cli <- curlEasyInit | Nothing => putStrLn "error in curlEasyInit"

    CURLE_OK <- curlEasySetopt cli CURLOPT_URL "https://google.com" | c => do
        putStrLn $ "error in curlEasySetopt: " ++ show c

    CURLE_OK <- curlEasyPerform cli | c => do
        putStrLn $ "error in curlEasyPerform: " ++ show c

    curlEasyCleanup cli
    curl_global_cleanup
