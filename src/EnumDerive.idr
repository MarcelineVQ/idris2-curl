module EnumDerive

import public Language.Reflection

%language ElabReflection

-- Tooling for basic interfaces of Enumeration types, e.g. data Foo = Biz | Baz

nameStr : Name -> String
nameStr (UN x) = x
nameStr (MN x y) = x
nameStr (NS xs x) = nameStr x
nameStr (DN x y) = x

eFC : FC
eFC = EmptyFC

export
conNames : Name -> Elab (List Name)
conNames x = do
  cons <- getCons x
  for cons $ \con => do
    [(con,ty)] <- getType con
      | _ => fail $ show x ++ " is not unique in scope."
    pure con

-- Provide a more custom list to skip some elements. e.g.
-- enums xs (ys = [0..12] ++ [14..])  would skip assigning 13
export
enums : (xs : List x) -> {default [0 .. cast (length xs)] ys : List Int}
     -> List (Int, x)
enums xs = flip zip xs ys

export
%macro
showEnum : Elab (x -> String)
showEnum = do
    Just (IPi _ _ _ n@(Just varnam) tyimp@(IVar _ ty) `(String)) <- goal
      | _ => fail "Required type is not: x -> String"
    cns <- conNames ty
    let clauses = clause <$> cns
        caser = ICase eFC (IVar eFC varnam) tyimp clauses
        lam = ILam eFC MW ExplicitArg n tyimp caser
    check lam
  where
    clause : Name -> Clause
    clause n = PatClause eFC (IVar eFC n) (IPrimVal eFC (Str (nameStr n)))

||| Provide a more custom list to skip some elements. e.g.
||| enumToInt ([0..12] ++ [14..30]) would skip assigning 13
export
%macro
enumToInt : List Int -> Elab (x -> Int)
enumToInt xs = do
    Just (IPi _ _ _ n@(Just varnam) tyimp@(IVar _ ty) `(Int)) <- goal
      | _ => fail "Required type is not: x -> Int"
    cns <- conNames ty
    False <- pure $ length xs < length cns
      | True => fail "Provided list is too short to cover all constructors."
    let clauses = clause <$> enums cns {ys = xs}
        caser = ICase eFC (IVar eFC varnam) tyimp clauses
        lam = ILam eFC MW ExplicitArg n tyimp caser
    check lam
  where
    clause : (Int, Name) -> Clause
    clause (i,n) = PatClause eFC (IVar eFC n) (IPrimVal eFC (I i))

-------------------------------------------------
-- Examples
-------------------------------------------------

data Foo = Biz | Baz

private
interface ToCode a where
  toCode : a -> Int

private
Show Foo where
  show = showEnum

private
ToCode Foo where
  toCode = enumToInt [0,1]

bif : toCode Baz == 1 = True
bif = Refl

fraf : show Baz == "Baz" = True
fraf = Refl
