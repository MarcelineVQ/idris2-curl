module Network.Curl.Types

import public Network.Curl.Types.Code
import public Network.Curl.Types.Option
import public Network.Curl.Types.CurlCode
import public Network.Curl.Types.SSL

export
data HandlePtr : Type where

export
data FunPtr : Type where

export
data ObjPtr : Type where

public export
data HandleType = Easy | Multi

public export
data CurlHandle : HandleType -> Type where
  MkH : (1 _ : Ptr HandlePtr) -> CurlHandle ty
