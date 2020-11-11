module Network.Curl.Global

import Network.Curl.Types
import Network.Curl.Prim.Global

||| We use CURL_GLOBAL_ALL since that's the only useful flag in this libcurl
||| version. CURL_GLOBAL_ALL = 3 on my system
export
curlGlobalInit : HasIO io => io CurlECode
curlGlobalInit = curl_global_init

export
curlGlobalCleanup : HasIO io => io ()
curlGlobalCleanup = curl_global_cleanup
