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
nameStr (RF n) = n

export
mapName : (String -> String) -> Name -> Name
mapName f (UN x) = UN (f x)
mapName f (MN x y) = MN (f x) y
mapName f (NS x y) = NS x (mapName f y)
mapName f (DN x y) = DN (f x) y
mapName f (RF n) = RF (f n)

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

export
getTTImpFC : TTImp -> FC
getTTImpFC (IVar fc _) = fc
getTTImpFC (IPi fc _ _ _ _ _) = fc
getTTImpFC (ILam fc _ _ _ _ _) = fc
getTTImpFC (ILet fc _ _ _ _ _) = fc
getTTImpFC (ICase fc _ _ _) = fc
getTTImpFC (ILocal fc _ _) = fc
getTTImpFC (IUpdate fc _ _) = fc
getTTImpFC (IApp fc _ _) = fc
getTTImpFC (INamedApp fc _ _ _) = fc
getTTImpFC (IAutoApp fc _ _) = fc
getTTImpFC (IWithApp fc _ _) = fc
getTTImpFC (ISearch fc _) = fc
getTTImpFC (IAlternative fc _ _) = fc
getTTImpFC (IRewrite fc _ _) = fc
getTTImpFC (IBindHere fc _ _) = fc
getTTImpFC (IBindVar fc _) = fc
getTTImpFC (IAs fc _ _ _) = fc
getTTImpFC (IMustUnify fc _ _) = fc
getTTImpFC (IDelayed fc _ _) = fc
getTTImpFC (IDelay fc _) = fc
getTTImpFC (IForce fc _) = fc
getTTImpFC (IQuote fc _) = fc
getTTImpFC (IQuoteName fc _) = fc
getTTImpFC (IQuoteDecl fc _) = fc
getTTImpFC (IUnquote fc _) = fc
getTTImpFC (IPrimVal fc _) = fc
getTTImpFC (IType fc) = fc
getTTImpFC (IHole fc _) = fc
getTTImpFC (Implicit fc _) = fc
getTTImpFC (IWithUnambigNames fc _ _) = fc
