module EnumDerive

import public Language.Reflection

%language ElabReflection

-- Tooling for basic interfaces of Enumeration types, e.g. data Foo = Biz | Baz
-- Use of this module automatically imports Language.Reflection and requires
-- %language ElabReflection to be enabled below your imports.
-- Examples at module bottom

-- nat casting is slow
intLength : List a -> Int
intLength [] = 0
intLength (_ :: xs) = 1 + intLength xs

guard : Bool -> String -> Elab ()
guard p s = if p then pure () else fail s

nameStr : Name -> String
nameStr (UN x) = x
nameStr (MN x y) = x
nameStr (NS xs x) = nameStr x
nameStr (DN x y) = x

eFC : FC
eFC = EmptyFC

export
constructors : Name -> Elab (List (Name, TTImp))
constructors x = do
  cons <- getCons x
  for cons $ \con => do
    [(con,ty)] <- getType con
      | _ => fail $ show con ++ " is not unique in scope."
    pure (con,ty)

export
conNames : Name -> Elab (List Name)
conNames x = map fst <$> constructors x

export
%macro
showEnum : Elab (x -> String)
showEnum = do
    Just (IPi _ _ _ _ tyimp@(IVar _ ty) `(String)) <- goal
      | _ => fail "Required type is not: x -> String"
    cns <- conNames ty
    check `(\lam => ~(ICase eFC `(lam) tyimp (clause <$> cns)))
  where
    clause : Name -> Clause
    clause n = PatClause eFC (IVar eFC n) (IPrimVal eFC (Str (nameStr n)))

export
%macro
eqEnum : Elab (x -> x -> Bool)
eqEnum = do
    Just (IPi _ _ _ _ (IVar _ ty1)
                      (IPi _ _ _ _ (IVar _ ty2)
                                  `(Prelude.Basics.Bool))) <- goal
      | _ => fail "Required type is not: x -> x -> Bool"
    guard (nameStr ty1 == nameStr ty2) "Required type is not: x -> x -> Bool"
    conns <- conNames ty1
    check `(\x,y => ~(casex conns `(x) `(y)))
  where
    -- pattern for a 'case': `(Biz => case ~y of Biz => True; _ => False)
    casey : (y : TTImp) -> (con : Name) -> Clause
    casey y con = PatClause eFC (IVar eFC con) $ ICase eFC y `(_)
      [ PatClause eFC (IVar eFC con) `(True)
      , PatClause eFC `( _ ) `(False)]
    -- `(case ~x of Biz => rpatterns y Biz; Baz => rpatterns y Baz)
    casex : (cons : List Name) -> (x : TTImp) -> (y : TTImp) -> TTImp
    casex cs x y = ICase eFC x `(_) (map (casey y) cs)
    {- casex: case x of
       casey:   Biz => case y of
                  Biz => True
                  _ => False -}

||| This orders enum constructors by their position in the datatype
||| data Foo = Biz | Baz     Biz < Baz
export
%macro
compareEnum : Elab (x -> x -> Ordering)
compareEnum = do
    Just (IPi _ _ _ _ (IVar _ t1)
                      (IPi _ _ _ _ (IVar _ t2)
                                  `(Prelude.EqOrd.Ordering))) <- goal
      | _ => fail "Required type is not: x -> x -> Ordering"
    guard (nameStr t1 == nameStr t2) "Required type is not: x -> x -> Ordering"
    cns <- conNames t1
    clauses <- traverse clause (zip [0 .. intLength cns] cns)
    let toInt = \x => ICase eFC x `(_) clauses
    check `(\a,b => compare {ty=Int} ~(toInt `(a)) ~(toInt `(b)))
  where
    clause : (Int, Name) -> Elab Clause
    clause (i,n) = pure $ PatClause eFC (IVar eFC n) !(quote i)

||| Assigns an Int value to each constructor.
||| Provide a more custom list to skip some elements. e.g.
||| enumToInt ([0..12] ++ [14..30]) would skip assigning 13
export
%macro
enumTo : List Int -> Elab (x -> Int)
enumTo xs = do
    Just (IPi _ _ _ _ tyimp@(IVar _ ty) `(Int)) <- goal
      | _ => fail "Required type is not: x -> Int"
    cns <- conNames ty
    False <- pure $ length xs < length cns
      | True => fail "Provided list is too short to cover all constructors."
    clauses <- traverse clause (zip xs cns)
    check `(\lam => ~(ICase eFC `(lam) tyimp clauses))
  where
    clause : (a, Name) -> Elab Clause
    clause (i,n) = pure $ PatClause eFC (IVar eFC n) !(quote i)

-------------------------------------------------
-- Examples
-------------------------------------------------

data Foo = Biz | Baz
data Foo2 = Fiz | Fizz | Fizzz

private
interface ToCode' a where
  toCode : a -> Int

private
interface FromCode' a where
  fromCode : Int -> Maybe a

private
Show Foo where
  show = showEnum

private
Eq Foo where
  (==) = eqEnum

private
Ord Foo where
  compare = compareEnum

private
ToCode' Foo where
  toCode = enumTo [0,1]

eqTest1 : Baz == Baz = True
eqTest1 = Refl

eqTest2 : Baz == Biz = False
eqTest2 = Refl

enumToTest1 : toCode Baz == 1 = True
enumToTest1 = Refl

showTest1 : show Baz == "Baz" = True
showTest1 = Refl

compareTest1 : Biz > Baz = False
compareTest1 = Refl

compareTest2 : Biz < Baz = True
compareTest2 = Refl

compareTest3 : compare Biz Baz = LT
compareTest3 = Refl
