module Network.Curl.Types.CurlMOption

import Network.Curl.Types.Code
import Network.Curl.Types.OptType
import public Network.Curl.Types.Handle

import Data.Buffer

import Derive.Enum
%language ElabReflection

public export
data CurlMOption : OptType -> Type where
  CURLMOPT_SOCKETFUNCTION : CurlMOption CURLOPTTYPE_FUNCTIONPOINT -- 1
  CURLMOPT_SOCKETDATA : CurlMOption CURLOPTTYPE_OBJECTPOINT
  CURLMOPT_PIPELINING : CurlMOption CURLOPTTYPE_LONG
  CURLMOPT_TIMERFUNCTION : CurlMOption CURLOPTTYPE_FUNCTIONPOINT
  CURLMOPT_TIMERDATA : CurlMOption CURLOPTTYPE_OBJECTPOINT
  CURLMOPT_MAXCONNECTS : CurlMOption CURLOPTTYPE_LONG
  CURLMOPT_MAX_HOST_CONNECTIONS : CurlMOption CURLOPTTYPE_LONG
  CURLMOPT_MAX_PIPELINE_LENGTH : CurlMOption CURLOPTTYPE_LONG
  CURLMOPT_CONTENT_LENGTH_PENALTY_SIZE : CurlMOption CURLOPTTYPE_OFF_T
  CURLMOPT_CHUNK_LENGTH_PENALTY_SIZE : CurlMOption CURLOPTTYPE_OFF_T
  CURLMOPT_PIPELINING_SITE_BL : CurlMOption CURLOPTTYPE_OBJECTPOINT
  CURLMOPT_PIPELINING_SERVER_BL : CurlMOption CURLOPTTYPE_OBJECTPOINT
  CURLMOPT_MAX_TOTAL_CONNECTIONS : CurlMOption CURLOPTTYPE_LONG
  CURLMOPT_PUSHFUNCTION : CurlMOption CURLOPTTYPE_FUNCTIONPOINT
  CURLMOPT_PUSHDATA : CurlMOption CURLOPTTYPE_OBJECTPOINT
  CURLMOPT_MAX_CONCURRENT_STREAMS : CurlMOption CURLOPTTYPE_LONG
  CURLMOPT_LASTENTRY : CurlMOption CURLOPTTYPE_LONG -- not used

[derivedToCode] ToCode (CurlMOption ty) where
  toCode = enumTo [1..17]

-- Automate toCode plussing with the power of named instances
public export
{ty : _} -> ToCode (CurlMOption ty) where
  toCode opt = toCode @{derivedToCode} opt + toCode ty

-- FromCode (CurlMOption ty) where
  -- fromCode = enumFrom [1..17]

export
Show (CurlMOption ty) where
  show = showEnum



public export
data CurlPollInfo : Type where
  CURL_POLL_NONE : CurlPollInfo
  CURL_POLL_IN : CurlPollInfo
  CURL_POLL_OUT : CurlPollInfo
  CURL_POLL_INOUT : CurlPollInfo
  CURL_POLL_REMOVE : CurlPollInfo

export
ToCode CurlPollInfo where
  toCode = enumTo [0..4]

FromCode CurlPollInfo where
  fromCode = enumFrom [0..4]

export
Show CurlPollInfo where
  show = showEnum

export
Eq CurlPollInfo where
  (==) = eqEnum

export
Ord CurlPollInfo where
  compare = compareEnum

public export
total
paramType : {ty : _} -> CurlMOption ty -> Type
paramType {ty = CURLOPTTYPE_FUNCTIONPOINT} CURLMOPT_SOCKETFUNCTION
  = Ptr HandlePtr -> (socket : Int) -> (pollinfo : Int) ->
    (socketdata : AnyPtr) -> (multi_assign : AnyPtr) -> PrimIO Int
paramType {ty = CURLOPTTYPE_FUNCTIONPOINT} CURLMOPT_TIMERFUNCTION
  = Ptr HandlePtr -> (timeout_ms : Int) -> AnyPtr -> PrimIO Int
paramType {ty = CURLOPTTYPE_FUNCTIONPOINT} CURLMOPT_PUSHFUNCTION
  = (parent : Ptr HandlePtr) -> (easy : Ptr HandlePtr) -> (num_headers : Int) ->
    (pushheaders_struct : AnyPtr) -> (userp : AnyPtr) -> PrimIO Int
-- paramType CURLMOPT_PIPELINING_SITE_BL = Void   -- not used by curl
-- paramType CURLMOPT_PIPELINING_SERVER_BL = Void -- not used by curl
-- paramType CURLMOPT_LASTENTRY = Void            -- not used by curl
paramType opt = optType ty
