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
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_STRINGPOINT} v
  = unsafeFromCode <$> primIO (prim_curl_easy_setopt_string h (toCode op) v)
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_FUNCTIONPOINT} v
  = map unsafeFromCode . primIO $ prim_curl_easy_setopt_fcallback h (toCode op) (\a,b,c,d => toPrim (v a b c d))
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OBJECTPOINT} v = ?dsf_3
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_SLISTPOINT} v = ?dsf_5
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OFF_T} v = ?dsf_6

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


curl_easy_escape : HasIO io => CurlHandle ty -> String -> io String
curl_easy_escape (MkH h) str = primIO $ prim_curl_easy_escape h str 0

-------------------------------------------------

-- This String is allocated by curl but idris ffi should copy it so we should be
-- safe in the presence or lack of curl_free.
-- TODO check this ^
-- We use Ptr String because idris String is null terminated, and will chop our
-- string if we're not careful
-- we need to curl_free Ptr String when we're done
-- char *curl_easy_unescape( CURL *curl, const char *url , int inlength, int *outlength );
%foreign "C:curl_easy_unescape,libcurl,curl/curl.h"
prim_curl_easy_unescape : Ptr HandlePtr -> (url : String) -> (url_len : Int)
                       -> AnyPtr -> PrimIO (Ptr String)

curl_easy_unescape : HasIO io => CurlHandle ty -> String -> io String
curl_easy_unescape (MkH h) str = do
  alloc 40 $ \ptr => do
    src <- primIO $ prim_curl_easy_unescape h str 0 ptr
    v <- foreign_ref_int ptr 0
    printLn v
    pure "dsffdaasdsa"

testo : IO ()
testo = do
  Just h <- curl_easy_init
    | _ => printLn "fuck"
  l <- curl_easy_escape h "fafodob"
  m <- curl_easy_unescape h l
  n <- curl_easy_escape h m
  printLn n
-------------------------------------------------

