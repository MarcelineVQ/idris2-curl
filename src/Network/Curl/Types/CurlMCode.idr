module Network.Curl.Types.CurlMCode

import  Network.Curl.Types.Code

import Util

import Derive.Enum
%language ElabReflection

public export
data CurlMCode
  = CURLM_CALL_MULTI_PERFORM -- -1  /* please call curl_multi_perform() or
                                  -- curl_multi_socket*() soon */
  | CURLM_OK
  | CURLM_BAD_HANDLE      -- /* the passed-in handle is not a valid CURLM handle */
  | CURLM_BAD_EASY_HANDLE -- /* an easy handle was not good/valid */
  | CURLM_OUT_OF_MEMORY   -- /* if you ever get this, you're in deep sh*t */
  | CURLM_INTERNAL_ERROR  -- /* this is a libcurl bug */
  | CURLM_BAD_SOCKET      -- /* the passed in socket argument did not match */
  | CURLM_UNKNOWN_OPTION  -- /* curl_multi_setopt() with unsupported option */
  | CURLM_ADDED_ALREADY   -- /* an easy handle already added to a multi handle was
                             -- attempted to get added - again */
  | CURLM_RECURSIVE_API_CALL -- /* an api function was called from inside a
                          --   callback */
  | CURLM_WAKEUP_FAILURE  -- /* wakeup is unavailable or failed */
  | CURLM_BAD_FUNCTION_ARGUMENT  -- /* function called with a bad parameter */
  | CURLM_LAST

export
Show CurlMCode where
  show = showEnum

export
Eq CurlMCode where
  (==) = eqEnum

export
Ord CurlMCode where
  compare = compareEnum

export
ToCode CurlMCode where
  toCode = enumTo $ [(-1)] ++ [0..11]

export
FromCode CurlMCode where
  unsafeFromCode = unsafeEnumFrom $ [(-1)] ++ [0..11]
  fromCode = enumFrom $ [(-1)] ++ [0..11]
