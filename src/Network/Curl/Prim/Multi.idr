module Network.Curl.Prim.Multi

-------------------------------------------------
-- Multi
-------------------------------------------------
-- curl_multi_add_handle
-- curl_multi_assign
-- curl_multi_cleanup
-- curl_multi_fdset
-- curl_multi_info_read
-- curl_multi_init
-- curl_multi_perform
-- curl_multi_remove_handle
-- curl_multi_setopt
-- curl_multi_socket_action
-- curl_multi_strerror
-- curl_multi_timeout
-- curl_multi_poll
-- curl_multi_wait
-- curl_multi_wakeup
-------------------------------------------------

-- import Network.Curl.Prim.Mem
import Network.Curl.Prim.Other

import Network.Curl.Types
-- import Derive.Enum

import Data.Buffer

import Language.Reflection
%language ElabReflection

-- curl_multi_init
%foreign "C:curl_multi_init,libcurl,curl/curl.h"
prim_curl_multi_init : PrimIO (Ptr HandlePtr)

export
curl_multi_init : HasIO io => io (Maybe (CurlHandle Multi))
curl_multi_init = do r <- primIO prim_curl_multi_init
                     pure $ if believe_me r == 0 then Nothing
                                                 else Just (MkH r)
-- curl_multi_cleanup
%foreign "C:curl_multi_cleanup,libcurl,curl/curl.h"
prim_curl_multi_cleanup : Ptr HandlePtr -> PrimIO ()

export
curl_multi_cleanup : HasIO io => CurlHandle Multi -> io ()
curl_multi_cleanup (MkH ptr) = primIO (prim_curl_multi_cleanup ptr)

{- curl_multi_socket_action
CURLMcode curl_multi_socket_action(CURLM * multi_handle,
                                   curl_socket_t sockfd,
                                   int ev_bitmask,
                                   int *running_handles);
-}
%foreign "C:curl_multi_socket_action,libcurl,curl/curl.h"
prim_curl_multi_socket_action : Ptr HandlePtr -> Int -> Int -> Ptr Int -> PrimIO (Ptr HandlePtr)

export
curl_multi_socket_action : HasIO io => CurlHandle Multi -> Int -> Int -> Ptr Int -> 
curl_multi_socket_action = ?SDfsfd



-- curl_multi_info_read


%foreign "C:curl_multi_setopt,libcurl,curl/curl.h"
prim_curl_multi_setopt_long : Ptr HandlePtr -> Int -> Int -> PrimIO Int

%foreign "C:curl_multi_setopt,libcurl,curl/curl.h"
prim_curl_multi_setopt_objptr : Ptr HandlePtr -> Int -> AnyPtr -> PrimIO Int

%foreign "C:curl_multi_setopt,libcurl,curl/curl.h"
prim_curl_multi_setopt_off_t : Ptr HandlePtr -> Int -> Bits64 -> PrimIO Int

%foreign "C:curl_multi_setopt,libcurl,curl/curl.h"
prim_curl_multi_setopt_blob : Ptr HandlePtr -> Int -> Buffer -> PrimIO Int

-- mSetOptPrim is to fill the role of
{-
%foreign "C:curl_multi_setopt,libcurl,curl/curl.h"
prim_curl_multi_setopt : Ptr HandlePtr -> Int -> any -> PrimIO CurlECode
-}
-- We can't pass 'any' or an arbitrary Type to a foreign function, but we can
-- generate that function at the type we need.
-- e.g. mSetOptPrim CURLMOPT_SOCKETFUNCTION would generate and declare:
--   %foreign "C:curl_multi_setopt,libcurl,curl/curl.h"
--   setOptPrim_CURLMOPT_SOCKETFUNCTION : Ptr HandlePtr -> Int
--     -> (HandlePtr -> Int -> Int -> AnyPtr -> AnyPtr -> PrimIO Int)
--     -> PrimIO Int
-- Specializing `any` to `(HandlePtr -> Int -> Int -> AnyPtr -> AnyPtr -> PrimIO Int)`
-- in accordance with the type given by `paramType CURLMOPT_SOCKETFUNCTION`

-- mSetOptPrim then also inserts the name of the generated prim at the use-site.
-- Below in `curl_multi_setopt` we use let to give it a place to generate to and
-- then use it. It wasn't happy about being placed directly where `prim` is
-- used instead of with the let. Possibly this is a parsing issue wih %runElab
-- or my own misunderstanding.
%macro
mSetOptPrim : {opty : _} -> (opt : CurlMOption opty)
           -> Elab (Ptr HandlePtr -> Int -> paramType opt -> PrimIO Int)
mSetOptPrim opt = do
  let name = UN $ "setOptPrim_" ++ show opt
  z <- quote (paramType opt)
  let ty = MkTy EmptyFC name `(Ptr HandlePtr -> Int -> ~z -> PrimIO Int)
  let claim = IClaim EmptyFC MW Private
                [ForeignFn ["C:curl_multi_setopt,libcurl,curl/curl.h"]] ty
  declare [claim] -- generate prim
  check (IVar EmptyFC name) -- insert prim's name

-- hideous but we only need to define it once
export
total
curl_multi_setopt : HasIO io => CurlHandle Multi -> {ty : _}
                 -> (opt : CurlMOption ty) -> paramType opt -> io CurlMCode
curl_multi_setopt (MkH h) opt@CURLMOPT_SOCKETFUNCTION v = do
    let prim = mSetOptPrim opt
    pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_multi_setopt (MkH h) opt@CURLMOPT_TIMERFUNCTION v = do
    let prim = mSetOptPrim opt
    pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_multi_setopt (MkH h) opt@CURLMOPT_PUSHFUNCTION v = do
    let prim = mSetOptPrim opt
    pure (unsafeFromCode !(primIO $ prim h (toCode opt) v))
curl_multi_setopt (MkH h) opt@CURLMOPT_PIPELINING v = pure $ -- special, bitmask, TODO, determine if we care
    unsafeFromCode !(primIO $ prim_curl_multi_setopt_long h (toCode opt) v)
curl_multi_setopt {ty = CURLOPTTYPE_LONG} (MkH h) opt v = pure $
    unsafeFromCode !(primIO $ prim_curl_multi_setopt_long h (toCode opt) v)
curl_multi_setopt {ty = CURLOPTTYPE_OBJECTPOINT} (MkH h) opt v = pure $
    unsafeFromCode !(primIO $ prim_curl_multi_setopt_objptr h (toCode opt) v)
curl_multi_setopt {ty = CURLOPTTYPE_OFF_T} (MkH h) opt v = pure $
    unsafeFromCode !(primIO $ prim_curl_multi_setopt_off_t h (toCode opt) v)
curl_multi_setopt {ty = CURLOPTTYPE_BLOB} (MkH h) opt v = pure $
    unsafeFromCode !(primIO $ prim_curl_multi_setopt_blob h (toCode opt) v)
curl_multi_setopt {ty = UnusedOptType} _ _ _ = pure CURLM_UNKNOWN_OPTION -- can't happen

useoptest : IO ()
useoptest = do
  Just r <- curl_multi_init
    | _ => printLn "foo"
  CURLM_OK <- curl_multi_setopt r CURLMOPT_SOCKETFUNCTION $ \h,sock,info,dat,sdat => toPrim $ do
      pure 0
    | o => printLn o
  -- curl_multi_setopt r CURLMOPT_LASTENTRY ?this_accepts_only_void
  putStrLn "was set"
  curl_multi_cleanup r
