module Network.Curl.Prim

import Network.Curl.Types

import public Network.Curl.Prim.Easy
import public Network.Curl.Prim.Global
import public Network.Curl.Prim.Mime
import public Network.Curl.Prim.Multi
import public Network.Curl.Prim.Other

import Data.IORef

import Data.List as List

import Derive.Prim

import Util

data HandlePtr : Type where
data FunPtr : Type where
data ObjPtr : Type where

public export
data HandleType = Easy | Multi

export
data CurlHandle : HandleType -> Type where
  MkH : (1 _ : Ptr HandlePtr) -> CurlHandle ty

data OptTag = LongTag | FunPtrTag | ObjPtrTag | OffTTag


%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_init : PrimIO (Ptr HandlePtr)

export
curl_easy_init : HasIO io => io (Maybe (CurlHandle Easy))
curl_easy_init = do r <- primIO prim_curl_easy_init
                    pure $ if believe_me r == 0 then Nothing
                                                else Just (MkH r)

%runElab makeHasIO "curl_easy_cleanup" Export
           ["C:curl_global_init,libcurl,curl/curl.h"]
          `[ prim_curl_easy_cleanup : Ptr HandlePtr -> PrimIO () ] --`

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

%foreign "C:curl_easy_perform,libcurl,curl/curl.h"
prim_curl_easy_perform : Ptr HandlePtr -> PrimIO Int

export
curl_easy_perform : HasIO io => CurlHandle Easy -> io CurlCode
curl_easy_perform (MkH ptr) = unsafeFromCode <$> primIO (prim_curl_easy_perform ptr)

-- every init must have a cleanup, but we can re-use things before cleanup too,
-- so allow for this
-- e.g. 'If you want to re-use an easy handle that was added to the multi handle
-- for transfer, you must first remove it from the multi stack and then re-add
-- it again'

-- all curl has to start with global init and and with global cleanup
-- each 'easy' has to then start with easy init and end with easy cleanup
-- 'multi' is many easy with a multiperform instead of easy perform
-- How do I enforce that multi eats all easy in scope?
-- Can I simply curl_multi_add_handle with a linear easy to eat it up?
-- runCurl : IO ()
-- runCurl = do
--   CURLE_OK <- curl_global_init
--     | r => putStrLn $ "global init not ok: " ++ show r
--   Just h <- curl_easy_init
--     | Nothing => putStrLn "dsf"
--   CURLE_OK <- curl_easy_setopt h CURLOPT_URL "http://example.com"
--     | r => putStrLn $ "setopt not ok: " ++ show r
--   Right b <- pure $ the (Either Char (List Bits8)) $ Right [1,2,3] -- easy_to_bytes h
--     | Left r => putStrLn $ "tobytes not ok: " ++ show r
--   -- printLn b
--   printLn $ foldr (\b,ss => strCons (cast (cast {to=Int} b)) ss) "" b
--   CURLE_OK <- curl_easy_perform h
--     | r => putStrLn $ "perform not ok: " ++ show r
--   curl_easy_cleanup h
-- 
  -- globalinit
  -- easyinit
  -- set url
  -- set where/how to save
  -- perform
  -- easycleanup
  -- globalcleanup
  -- ?runCurl_rhs

-- It would be a good idea to query for all the settings on global init so we
-- can save and restore them for certain operations, sort of a ReaderT 'local'
-- kind of thing maybe



