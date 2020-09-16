module Network.Curl.Prim.Global

-------------------------------------------------
-- Global
-------------------------------------------------
-- curl_global_cleanup
-- curl_global_init
-- curl_global_init_mem -- not implemented
-- curl_global_sslset -- not implemented
-------------------------------------------------

import Network.Curl.Types

import System.FFI


import Derive.Prim
%language ElabReflection

export
%foreign "C:curl_global_init,libcurl,curl/curl.h"
prim_curl_global_init : Int -> PrimIO Int

||| We use CURL_GLOBAL_ALL since that's the only useful flag in this libcurl
||| version. CURL_GLOBAL_ALL = 3 on my system
export
curl_global_init : HasIO io => io CurlECode
curl_global_init = unsafeFromCode <$> primIO (prim_curl_global_init 3)

%runElab makeHasIO "curl_global_cleanup" Export
          `[ %foreign "C:curl_global_init,libcurl,curl/curl.h"
             export
             prim_curl_global_cleanup : PrimIO () ] --`
