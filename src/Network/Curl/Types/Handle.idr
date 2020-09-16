module Network.Curl.Types.Handle

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
