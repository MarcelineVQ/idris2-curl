module Network.Curl.Prim

-- import Data.Bytes.Lazy

import Control.App
import Network.Curl.Types

-- import Data.Bytes.Strict
-- import Data.Word.Word8

import Data.IORef

import Data.List as List

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
-- Multi
-------------------------------------------------





-- paramTy : OptType -> Type
-- paramTy LongTag = Int
-- paramTy FunPtrTag = Ptr FunPtr
-- paramTy ObjPtrTag = Ptr ObjPtr
-- paramTy OffTTag = AnyPtr

-- data CurlOption : OptTag -> Type where
  -- Opt1 : Int -> CurlOption LongTag
  -- Opt2 : Int -> CurlOption FunPtrTag

data Fraf : Type where [external]

%foreign "C:curl_global_init,libcurl,curl/curl.h"
prim_curl_global_init : Int -> PrimIO Int

||| We use CURL_GLOBAL_ALL since that's the only useful flag in this libcurl version
||| CURL_GLOBAL_ALL = 3 on my system
export
curl_global_init : HasIO io => io CurlCode
curl_global_init = fromCode <$> primIO (prim_curl_global_init 3)

%foreign "C:curl_global_init,libcurl,curl/curl.h"
prim_curl_global_cleanup : PrimIO ()

export
curl_global_cleanup : HasIO io => io ()
curl_global_cleanup = primIO prim_curl_global_cleanup



%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_init : PrimIO (Ptr HandlePtr)

export
curl_easy_init : HasIO io => io (Maybe (CurlHandle Easy))
curl_easy_init = do r <- primIO prim_curl_easy_init
                    pure $ if believe_me r == 0 then Nothing
                                                else Just (MkH r)

%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_cleanup : Ptr HandlePtr -> PrimIO ()

export
curl_easy_cleanup : HasIO io => CurlHandle Easy -> io ()
curl_easy_cleanup (MkH ptr) = primIO (prim_curl_easy_cleanup ptr)

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
  = fromCode <$> primIO (prim_curl_easy_setopt_long h (toCode op) v)
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_STRINGPOINT} v
  = fromCode <$> primIO (prim_curl_easy_setopt_string h (toCode op) v)
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_FUNCTIONPOINT} v
  = map fromCode . primIO $ prim_curl_easy_setopt_fcallback h (toCode op) (\a,b,c,d => toPrim (v a b c d))
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OBJECTPOINT} v = ?dsf_3
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_SLISTPOINT} v = ?dsf_5
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OFF_T} v = ?dsf_6

  -- = fromCode <$> primIO (prim_curl_easy_setopt x (toCode y) (believe_me z))

%foreign "C:curl_easy_perform,libcurl,curl/curl.h"
prim_curl_easy_perform : Ptr HandlePtr -> PrimIO Int

export
curl_easy_perform : HasIO io => CurlHandle Easy -> io CurlCode
curl_easy_perform (MkH ptr) = fromCode <$> primIO (prim_curl_easy_perform ptr)

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



