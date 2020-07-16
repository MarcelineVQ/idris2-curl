module Network.Curl.Prim

import Data.Bytes.Lazy

import Control.App
import Network.Curl.Types
import Network.Curl.Option

import Data.Bytes.Strict
import Data.Word.Word8

import Data.IORef

import Data.List as List

import Util

data HandlePtr : Type where
data FunPtr : Type where
data ObjPtr : Type where

data CurlHandle : Type where
  MkH : (1 _ : Ptr HandlePtr) -> CurlHandle

data OptTag = LongTag | FunPtrTag | ObjPtrTag | OffTTag

-- paramTy : OptType -> Type
-- paramTy LongTag = Int
-- paramTy FunPtrTag = Ptr FunPtr
-- paramTy ObjPtrTag = Ptr ObjPtr
-- paramTy OffTTag = AnyPtr

-- data CurlOption : OptTag -> Type where
  -- Opt1 : Int -> CurlOption LongTag
  -- Opt2 : Int -> CurlOption FunPtrTag

%foreign "C:curl_global_init,libcurl,curl/curl.h"
prim_curl_global_init : Int -> PrimIO Int

||| We use CURL_GLOBAL_ALL since that's the only useful flag in this libcurl version
||| CURL_GLOBAL_ALL = 3 on my system
curl_global_init : HasIO io => io CurlCode
curl_global_init = fromCode <$> primIO (prim_curl_global_init 3)

%foreign "C:curl_global_init,libcurl,curl/curl.h"
prim_curl_global_cleanup : PrimIO ()

curl_global_cleanup : HasIO io => io ()
curl_global_cleanup = primIO prim_curl_global_cleanup



%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_init : PrimIO (Ptr HandlePtr)

curl_easy_init : HasIO io => io (Maybe CurlHandle)
curl_easy_init = do r <- primIO prim_curl_easy_init
                    pure $ if believe_me r == 0 then Nothing
                                                else Just (MkH r)

%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_cleanup : Ptr HandlePtr -> PrimIO ()

curl_easy_cleanup : HasIO io => CurlHandle -> io ()
curl_easy_cleanup (MkH ptr) = primIO (prim_curl_easy_cleanup ptr)

%foreign "C:curl_easy_strerror,libcurl,curl/curl.h"
curl_easy_strerror : (curlcode : Int) -> PrimIO String

-- It's just a lookup, expect this to work unless curl itself is broken
curlEasyStrError : CurlCode -> String
curlEasyStrError c = unsafePerformIO $ primIO $ curl_easy_strerror (toCode c)

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

-- 

-- curl_easy_setopt_fcallback : Ptr HandlePtr -> Int -> (AnyPtr -> Int -> Int -> AnyPtr -> IO Int) -> PrimIO Int
-- curl_easy_setopt_fcallback h y f z = ?curl_easy_setopt_fcallback_rhs  
  

-- size_t write_callback(char *ptr, size_t size, size_t nmemb, void *userdata);


curl_easy_setopt : HasIO io => {ty : _} -> CurlHandle -> CurlOption ty -> paramTy ty -> io CurlCode
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_LONG} v
  = fromCode <$> primIO (prim_curl_easy_setopt_long h (toCode op) v)
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_STRINGPOINT} v
  = fromCode <$> primIO (prim_curl_easy_setopt_string h (toCode op) v)
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_FUNCTIONPOINT} v
  = map fromCode . primIO $ prim_curl_easy_setopt_fcallback h (toCode op) (\a,b,c,d => toPrim (v a b c))
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OBJECTPOINT} v = ?dsf_3
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_SLISTPOINT} v = ?dsf_5
curl_easy_setopt (MkH h) op {ty = CURLOPTTYPE_OFF_T} v = ?dsf_6

  -- = fromCode <$> primIO (prim_curl_easy_setopt x (toCode y) (believe_me z))

%foreign "C:curl_easy_perform,libcurl,curl/curl.h"
prim_curl_easy_perform : Ptr HandlePtr -> PrimIO Int

curl_easy_perform : HasIO io => CurlHandle -> io CurlCode
curl_easy_perform (MkH ptr) = fromCode <$> primIO (prim_curl_easy_perform ptr)

partial -- BS.concat
easy_to_bytes : CurlHandle -> IO (Either CurlCode Bytes)
easy_to_bytes h = do
  ref <- newIORef {a=List Bytes} []
  CURLE_OK <- curl_easy_setopt h CURLOPT_WRITEFUNCTION $ \dat,s,len => do
      putStrLn dat
      let (b,_) = stringToBytes dat (s*len)
      modifyIORef ref (b ::)
      pure (s*len)
    | r => do putStrLn $ "setopt not ok: " ++ show r
              pure (Left r)
  CURLE_OK <- curl_easy_perform h
    | r => do putStrLn $ "perform not ok: " ++ show r
              pure (Left r)
  res <- readIORef ref
  pure $ Right $ concat (List.reverse res)



{-

#include <curl/curl.h>
 
size_t write_callback(char *ptr, size_t size, size_t nmemb, void *userdata);
 
CURLcode curl_easy_setopt(CURL *handle, CURLOPT_WRITEFUNCTION, write_callback);

-}


-- %foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
-- curl_easy_setopt : CurlHandle -> CurlOption opt_ty -> (param : paramTy opt_ty) -> PrimIO (Maybe CurlHandle)

data CurlOpt : Type where
  WRITEFUNCTION : Int -> CurlOpt

Semigroup CurlOpt where
  x <+> y = ?sdff

data CurlState = GOpen | GClosed | EasyOpen | EasyClosed

data CurlError = Faf

interface Has [Exception CurlError] e => CurlIO e where
  newGlobal : (1 p : (1 s : Int ) -> App e CurlCode) -> App e CurlCode
  withEasy : App e ()

-- every init must have a cleanup, but we can re-use things before cleanup too,
-- so allow for this
-- e.g. 'If you want to re-use an easy handle that was added to the multi handle
-- for transfer, you must first remove it from the multi stack and then re-add
-- it again'

fef : CurlState -> List CurlState -> Bool

data RunCurl : List CurlState -> List CurlState -> Type -> Type where
  GlobalInit : (b : Bool) -> RunCurl [] (case b of False => []
                                                   True => [GOpen]) ()
  EasyInit : (b : Bool) -> RunCurl [GOpen] (case b of False => []
                                                      True => EasyOpen :: [GOpen]) ()
  -- EasyCleanup : RunCurl (GOpen :: xs)
  GlobalCleanup : RunCurl [GOpen] [] ()
  Bind : RunCurl s1 s2 a -> (a -> RunCurl s1 s2 b) -> RunCurl s1 s2 b
  Pure : a -> RunCurl s1 s2 a

(>>=) : RunCurl s1 s2 a -> (a -> RunCurl s1 s2 b) -> RunCurl s1 s2 b
(>>=) = Bind

pure : a -> RunCurl s1 s2 a
pure = Pure

-- faf : RunCurl

globalinit : RunCurl [] [GOpen] ()

-- all curl has to start with global init and and with global cleanup
-- each easy has to then start with easy init and end with easy cleanup
-- multi is many easy with a multiperform instead of easy perform
partial
runCurl : IO ()
runCurl = do
  CURLE_OK <- curl_global_init
    | r => putStrLn $ "global init not ok: " ++ show r
  Just h <- curl_easy_init
    | Nothing => putStrLn "dsf"
  CURLE_OK <- curl_easy_setopt h CURLOPT_URL "http://example.com"
    | r => putStrLn $ "setopt not ok: " ++ show r
  Right b <- easy_to_bytes h
    | Left r => putStrLn $ "tobytes not ok: " ++ show r
  printLn b
  printLn $ foldr (\b,ss => strCons (cast (cast {to=Int} b)) ss) "" b
  CURLE_OK <- curl_easy_perform h
    | r => putStrLn $ "perform not ok: " ++ show r
  curl_easy_cleanup h
  
  -- globalinit
  -- easyinit
  -- set url
  -- set where/how to save
  -- perform
  -- easycleanup
  -- globalcleanup
  -- ?runCurl_rhs

partial
main : IO ()
main = runCurl

-- It would be a good idea to query for all the settings on global init so we
-- can save and restore them for certain operations



