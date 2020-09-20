module Network.Curl.Types.OptType

import Data.Buffer

import Network.Curl.Types.Code

import Derive.Enum
%language ElabReflection

public export
data OptType
  = UnusedOptType  -- -10, not part of curl, convenience
  | CURLOPTTYPE_LONG -- at east 32 bits -- 0
  | CURLOPTTYPE_OBJECTPOINT             -- 10000
  | CURLOPTTYPE_FUNCTIONPOINT -- void*  -- 20000
  | CURLOPTTYPE_OFF_T -- int64          -- 30000
  | CURLOPTTYPE_BLOB -- ???             -- 40000
  -- | CURLOPTTYPE_STRINGPOINT -- char*    -- 10000 -- aliases for OBJECTPOINT
  -- | CURLOPTTYPE_SLISTPOINT              -- 10000 -- aliases for OBJECTPOINT

curlOptTypeEnumList : List Int
curlOptTypeEnumList = [-10,0,10000,20000,30000,40000]

export
ToCode OptType where
  toCode = enumTo curlOptTypeEnumList

export
FromCode OptType where
  fromCode = enumFrom curlOptTypeEnumList

public export
optType : OptType -> Type
optType UnusedOptType = Void
optType CURLOPTTYPE_LONG = Int
optType CURLOPTTYPE_OBJECTPOINT = AnyPtr
optType CURLOPTTYPE_FUNCTIONPOINT = AnyPtr
optType CURLOPTTYPE_OFF_T = Bits64 -- maybe
optType CURLOPTTYPE_BLOB = Buffer -- I truly don't know what a blob is
