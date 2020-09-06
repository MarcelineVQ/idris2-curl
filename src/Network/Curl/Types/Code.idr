module Network.Curl.Types.Code

public export
interface ToCode a where
  toCode : a -> Int

public export
interface FromCode a where
  fromCode : Int -> a
