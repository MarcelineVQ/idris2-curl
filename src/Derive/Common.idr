module Derive.Common

import public Language.Reflection

-- nat casting is slow
export
intLength : List a -> Int
intLength = foldl (\xs,_ => 1 + xs) 0

export
eFC : FC
eFC = EmptyFC

export
toIVar : Name -> TTImp
toIVar = IVar eFC

export
toIBindVar : String -> TTImp
toIBindVar = IBindVar eFC

export
guard : Bool -> String -> Elab ()
guard p s = if p then pure () else fail s

export
nameStr : Name -> String
nameStr (UN x) = x
nameStr (MN x y) = x
nameStr (NS xs x) = nameStr x
nameStr (DN x y) = x

export
mapName : (String -> String) -> Name -> Name
mapName f (UN x) = UN (f x)
mapName f (MN x y) = MN (f x) y
mapName f (NS x y) = NS x (mapName f y)
mapName f (DN x y) = DN (f x) y

export
lookupType : Name -> Elab (Name, TTImp)
lookupType n = do
  [res@(qname,ttimp)] <- getType n
    | _ => fail $ show n ++ " is not unique in scope."
  pure res

export
constructors : Name -> Elab (List (Name, TTImp))
constructors x = do
  cons <- getCons x
  for cons lookupType

export
conNames : Name -> Elab (List Name)
conNames x = map fst <$> constructors x

export
argCount : TTImp -> Nat
argCount (IPi _ _ _ _ _ retty) = S (argCount retty)
argCount retty = Z
