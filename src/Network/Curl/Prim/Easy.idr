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

-- import Network.Curl.Prim.Mem
import Network.Curl.Prim.Other

import Network.Curl.Types
-- import Network.Curl.Types.CurlEOption as EO
-- import Derive.Enum

import Derive.Prim
%language ElabReflection


-------------------------------------------------

-- curl_easy_duphandle
%runElab makeHasIO "curl_easy_duphandle" Export
          `[ %foreign "C:curl_easy_duphandle,libcurl,curl/curl.h"
             export
             prim_curl_easy_duphandle : Ptr HandlePtr -> PrimIO (Ptr HandlePtr) ] --`

-------------------------------------------------


%foreign "C:curl_easy_init,libcurl,curl/curl.h"
prim_curl_easy_init : PrimIO (Ptr HandlePtr)

export
curl_easy_init : HasIO io => io (Maybe (CurlHandle Easy))
curl_easy_init = do r <- primIO prim_curl_easy_init
                    pure $ if believe_me r == 0 then Nothing
                                                else Just (MkH r)

-------------------------------------------------


%foreign "C:curl_easy_cleanup,libcurl,curl/curl.h"
prim_curl_easy_cleanup : Ptr HandlePtr -> PrimIO ()

export
curl_easy_cleanup : HasIO io => CurlHandle Easy -> io ()
curl_easy_cleanup (MkH h) = primIO $ prim_curl_easy_cleanup h



-- It's just a lookup, expect this to work unless curl itself is broken
%foreign "C:curl_easy_strerror,libcurl,curl/curl.h"
curl_easy_strerror : (curlcode : Int) -> String

export
curlEasyStrError : CurlECode -> String
curlEasyStrError c = curl_easy_strerror (toCode c)

-------------------------------------------------


%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_long : Ptr HandlePtr -> Int -> Int -> PrimIO Int

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_objptr : Ptr HandlePtr -> Int -> AnyPtr -> PrimIO Int

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_off_t : Ptr HandlePtr -> Int -> Bits64 -> PrimIO Int

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_blob : Ptr HandlePtr -> Int -> Buffer -> PrimIO Int

%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt_string : Ptr HandlePtr -> Int -> String -> PrimIO Int

-- This is to fill the role of
{-
%foreign "C:curl_easy_setopt,libcurl,curl/curl.h"
prim_curl_easy_setopt : Ptr HandlePtr -> Int -> any -> PrimIO CurlECode
-}
-- We can't pass 'any' or an arbitrary Type to a foreign function, but we can
-- generate that function at the type we need.
%macro
eSetOptPrim : {opty : _} -> (opt : CurlEOption opty)
           -> Elab (Ptr HandlePtr -> Int -> paramType opt -> PrimIO Int)
eSetOptPrim opt = do
  let name = UN $ "setOptPrim_" ++ show opt
  z <- quote (paramType opt)
  str <- quote "C:curl_easy_setopt,libcurl,curl/curl.h"
  let ty = MkTy EmptyFC EmptyFC name `(Ptr HandlePtr -> Int -> ~z -> PrimIO Int)
  let claim = IClaim EmptyFC MW Private
                [ForeignFn [str]] ty
  declare [claim] -- generate prim
  check (IVar EmptyFC name) -- insert prim's name

||| It's kind of unfortunate we need to case all of these `opt` when the code
||| used on them is exactly the same and while it could be done with elaboration
||| it's not really worth the work. Users shouldn't actually be exposed to this
||| definition to be scared away by it anyway.
export
curl_easy_setopt : HasIO io => CurlHandle Easy -> {ty : _}
                -> (opt : CurlEOption ty) -> paramType opt -> io CurlECode
curl_easy_setopt (MkH h) opt@CURLOPT_WRITEFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_READFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_PROGRESSFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_HEADERFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_DEBUGFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_SSL_CTX_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_IOCTLFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_CONV_FROM_NETWORK_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_CONV_TO_NETWORK_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_CONV_FROM_UTF8_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_SOCKOPTFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_OPENSOCKETFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_SEEKFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_SSH_KEYFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_INTERLEAVEFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_CHUNK_BGN_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_CHUNK_END_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_FNMATCH_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_CLOSESOCKETFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_XFERINFOFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_RESOLVER_START_FUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt (MkH h) opt@CURLOPT_TRAILERFUNCTION v = do
      let prim = eSetOptPrim opt
      pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_easy_setopt {ty = CURLOPTTYPE_LONG} (MkH h) opt v = pure $
  unsafeFromCode !(primIO $ prim_curl_easy_setopt_long h (toCode opt) v)
curl_easy_setopt {ty = CURLOPTTYPE_OBJECTPOINT} (MkH h) opt v = pure $
  unsafeFromCode !(primIO $ prim_curl_easy_setopt_objptr h (toCode opt) v)
curl_easy_setopt {ty = CURLOPTTYPE_OFF_T} (MkH h) opt v = pure $
  unsafeFromCode !(primIO $ prim_curl_easy_setopt_off_t h (toCode opt) v)
curl_easy_setopt {ty = CURLOPTTYPE_BLOB} (MkH h) opt v = pure $
  unsafeFromCode !(primIO $ prim_curl_easy_setopt_blob h (toCode opt) v)
curl_easy_setopt {ty = CURLOPTTYPE_STRINGPOINT} (MkH h) opt v = pure $
  unsafeFromCode !(primIO $ prim_curl_easy_setopt_string h (toCode opt) v)
curl_easy_setopt {ty = UnusedOptType} _ _ _ = pure CURLE_UNKNOWN_OPTION
--                      ^can't happen during normal use

-------------------------------------------------


%foreign "C:curl_easy_perform,libcurl,curl/curl.h"
prim_curl_easy_perform : Ptr HandlePtr -> PrimIO Int

export
curl_easy_perform : HasIO io => CurlHandle Easy -> io CurlECode
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

{-

-- This String is allocated by curl but idris ffi should copy it so we should be
-- safe in the presence or lack of curl_free.
-- TODO check this ^
-- char *curl_easy_unescape( CURL *curl, const char *url , int inlength, int *outlength );
%foreign "C:curl_easy_unescape,libcurl,curl/curl.h"
prim_curl_easy_unescape : Ptr HandlePtr -> (url : String) -> (url_len : Int)
                       -> (intptr : Buffer) -> PrimIO String

%foreign "C:curl_easy_unescape,libcurl,curl/curl.h"
prim_curl_easy_unescape' : Ptr HandlePtr -> (url : String) -> (url_len : Int)
                        -> Ptr Int -> PrimIO String



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

-}