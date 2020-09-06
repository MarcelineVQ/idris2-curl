module Network.Curl.Types.Code

||| Providing a mapping from constructor to Int.
public export
interface ToCode a where
  total
  toCode : a -> Int

||| Providing a mapping from Int to constructor.
||| Two ways in case you are certain the Int has a mapping.
public export
interface FromCode a where
  unsafeFromCode : Int -> a
  total
  fromCode : Int -> Maybe a
