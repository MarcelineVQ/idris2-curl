module Network.Curl.Prim.Easy

-------------------------------------------------
-- Easy
-------------------------------------------------
-- curl_easy_cleanup
-- curl_easy_duphandle
-- curl_easy_escape
-- curl_easy_getinfo
-- curl_easy_init
-- curl_easy_option_by_id
-- curl_easy_option_by_name
-- curl_easy_option_next
-- curl_easy_pause
-- curl_easy_perform
-- curl_easy_recv
-- curl_easy_reset
-- curl_easy_send
-- curl_easy_setopt
-- curl_easy_strerror
-- curl_easy_unescape
-- curl_easy_upkeep
-------------------------------------------------

import Data.Buffer

import Network.Curl.Prim.Mem
import Network.Curl.Prim.Other

import Network.Curl.Types
-- import Derive.Enum

import Derive.Prim
%language ElabReflection

-- curl_easy_cleanup
%runElab makeHasIO "curl_easy_cleanup" Export
           ["C:curl_easy_cleanup,libcurl,curl/curl.h"]
          `[ prim_curl_easy_cleanup : Ptr HandlePtr -> PrimIO () ] --`

-------------------------------------------------


-- curl_easy_duphandle
%runElab makeHasIO "curl_easy_duphandle" Export
           ["C:curl_easy_duphandle,libcurl,curl/curl.h"]
          `[ prim_curl_easy_duphandle : Ptr HandlePtr -> PrimIO (Ptr HandlePtr) ] --`

-------------------------------------------------


%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_init : PrimIO (Ptr HandlePtr)

export
curl_easy_init : HasIO io => io (Maybe (CurlHandle Easy))
curl_easy_init = do r <- primIO prim_curl_easy_init
                    pure $ if believe_me r == 0 then Nothing
                                                else Just (MkH r)

-------------------------------------------------


-- %foreign "C:curl_easy_cleanup,libcurl,curl/curl.h"
-- prim_curl_easy_cleanup : Ptr HandlePtr -> PrimIO ()
-- 
-- export
-- curl_easy_cleanup : HasIO io => CurlHandle Easy -> io ()
-- curl_easy_cleanup (MkH ptr) = primIO (prim_curl_easy_cleanup ptr)

-- It's just a lookup, expect this to work unless curl itself is broken
%foreign "C:curl_easy_strerror,libcurl,curl/curl.h"
curl_easy_strerror : (curlcode : Int) -> String

export
curlEasyStrError : CurlCode -> String
curlEasyStrError c = curl_easy_strerror (toCode c)

-------------------------------------------------


 -- -> IO LBytes

-- we can use IORef as an accumulator for this
-- write_callback : (Buffer -> Int -> Int -> AnyPtr -> IO Int) -> IO Int
-- write_callback f = ?write_callback_rhs

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_string : Ptr HandlePtr -> Int -> String -> PrimIO Int

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_long : Ptr HandlePtr -> Int -> Int -> PrimIO Int

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_fcallback : Ptr HandlePtr -> Int -> (String -> Int -> Int -> AnyPtr -> PrimIO Int) -> PrimIO Int

-- curl_easy_setopt_fcallback : Ptr HandlePtr -> Int -> (AnyPtr -> Int -> Int -> AnyPtr -> IO Int) -> PrimIO Int
-- curl_easy_setopt_fcallback h y f z = ?curl_easy_setopt_fcallback_rhs  

export
curl_easy_setopt : HasIO io => {ty : _} -> CurlHandle Easy -> CurlOption ty -> paramTy ty -> io CurlCode
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_LONG} v
  = unsafeFromCode <$> primIO (prim_curl_easy_setopt_long h (toCode op) v)
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_FUNCTIONPOINT} v
  = map unsafeFromCode . primIO $ prim_curl_easy_setopt_fcallback h (toCode op) (\a,b,c,d => toPrim (v a b c d))
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OBJECTPOINT} v = ?dsf_3
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OFF_T} v = ?dsf_6
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_BLOB} v = ?dsf_6s

  -- = fromCode <$> primIO (prim_curl_easy_setopt x (toCode y) (believe_me z))

-------------------------------------------------


%foreign "C:curl_easy_perform,libcurl,curl/curl.h"
prim_curl_easy_perform : Ptr HandlePtr -> PrimIO Int

export
curl_easy_perform : HasIO io => CurlHandle Easy -> io CurlCode
curl_easy_perform (MkH ptr) = unsafeFromCode <$> primIO (prim_curl_easy_perform ptr)

-------------------------------------------------


-- This String is allocated by curl but idris ffi should copy it so we should be
-- safe in the presence or lack of curl_free
-- TODO check this ^
-- TODO check for possible issues with \0
-- char *curl_easy_escape( CURL *curl, const char *string , int length );
%foreign "C:curl_easy_escape,libcurl,curl/curl.h"
prim_curl_easy_escape : Ptr HandlePtr -> (url : String)
                     -> (url_len : Int) -> PrimIO String

-- idris calls free after copying the PrimIO String it converts but curl wants you
-- to call curl_free, how can I reconcile this?
||| Escapes a String byte-by-byte without awareness of encoding.
||| String is expected to be free of \0 since strlen() is used by curl and so
||| will truncate on \0. This shouldn't be a problem in idris since the
||| primitive String already truncate on \0 in idris.
curl_easy_escape : HasIO io => CurlHandle ty -> String -> io String
curl_easy_escape (MkH h) str = primIO $ prim_curl_easy_escape h str 0
-- 0 is to tell curl to compute the length itself via strlen()

-------------------------------------------------

-- This String is allocated by curl but idris ffi should copy it so we should be
-- safe in the presence or lack of curl_free.
-- TODO check this ^
-- char *curl_easy_unescape( CURL *curl, const char *url , int inlength, int *outlength );
%foreign "C:curl_easy_unescape,libcurl,curl/curl.h"
prim_curl_easy_unescape : Ptr HandlePtr -> (url : String) -> (url_len : Int)
                       -> (intptr : Buffer) -> PrimIO String

||| Unescapes a String byte-by-byte without awareness of encoding. When
||| encountering %00 or some other source of \0 it returns as much of the String
||| as it got to that point.

||| NB I'm not bothering with proper handling %00, if you need this I'll accept
||| a tested PR though. It's just that idris's \0 terminated Strings can make
||| this a hassle. I'll revisit this when/if I made a Text type since it won't
||| have a weakness to \0.
curl_easy_unescape : HasIO io => CurlHandle ty -> String -> io (Either String String)
curl_easy_unescape (MkH h) str
  = withAllocElems {a=Int} 1 $ \iptr => do
      str <- primIO $ prim_curl_easy_unescape h str 0 (unsafeForeignPtrToBuffer iptr)
      len <- peek iptr
      pure $ if len > 0 && cast (length str) /= len -- str contained %00
        then Left str -- This is where a Text type could help.
        else Right str

testo : IO ()
testo = do
  Just h <- curl_easy_init
    | _ => printLn "fuck"
  l <- curl_easy_escape h "fafodoba"
  Right m <- curl_easy_unescape h l
    | Left s => printLn s -- print remainder if there is one
  n <- curl_easy_escape h m
  printLn n
  Right m <- curl_easy_unescape h "gabbo%00nock"
    | Left s => printLn s -- print remainder if there is one
  printLn m

-------------------------------------------------
