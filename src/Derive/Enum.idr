module Derive.Enum

import Data.Nat -- LTE example

import public Language.Reflection
%language ElabReflection

-- Tooling for basic interfaces of Enumeration types, e.g. data Foo = Biz | Baz
-- This should also support indexed enum types, because our operations are just
-- about the constructors.
-- Use of this module automatically imports Language.Reflection and requires
-- %language ElabReflection to be enabled below your imports.
-- Examples at module bottom

-- The extra constructor checks to ensure Enumeration type can add a fair chunk to
-- the compile time, best to place your types you're deriving for into a module
-- that doesn't change often.



-------------------------------------------------
-- Helpers, moving these to another module breaks Elaboration :S
-------------------------------------------------

-- nat casting is slow
intLength : List a -> Int
intLength = foldl (\xs,_ => 1 + xs) 0

nameStr : Name -> String
nameStr (UN x) = x
nameStr (MN x y) = x
nameStr (NS xs x) = nameStr x
nameStr (DN x y) = x

eFC : FC
eFC = EmptyFC

guard : Bool -> String -> Elab ()
guard p s = if p then pure () else fail s

lookupType : Name -> Elab (Name, TTImp)
lookupType n = do
  [res@(qname,ttimp)] <- getType n
    | _ => fail $ show n ++ " is not unique in scope."
  pure res

constructors : Name -> Elab (List (Name, TTImp))
constructors x = do
  cons <- getCons x
  for cons lookupType
 
conNames : Name -> Elab (List Name)
conNames x = map fst <$> constructors x

-------------------------------------------------

checkList : List a -> List b -> Elab ()
checkList xs ys =
  case compare (length xs) (length ys) of
    LT => fail "Provided list is too short to exactly cover all constructors."
    GT => fail "Provided list is too long to exactly cover all constructors."
    EQ => pure ()

enumName : TTImp -> Maybe Name
enumName (IVar _ n) = Just n
enumName (IApp _ (IVar _ n) r) = Just n
enumName _ = Nothing

-- An enum type's constructors are all non-explicit or just an ivar, and its
-- type is either the first argument of an application or an ivar.
-- e.g. A : {g : Nat} -> Foo g  
--      B : Foo Z
--      C : Foo Z
-- e.g. data Foo : Nat -> Type where
--      data Foo : Type where
isEnumType : Name -> Elab Bool
isEnumType n = do
    cons <- constructors n
    pure $ all isEnumCon (map snd cons)
  where
    isEnumCon : TTImp -> Bool
    isEnumCon (IPi _ _ ExplicitArg _ _ retty) = False
    isEnumCon (IPi _ _ _ _ _ retty) = isEnumCon retty
    isEnumCon _ = True -- we got to the end without explicit args

-- combine our checks into one spot:
-- 1. is a given TTImp representing a type
-- 2. do its constructors have no explicit arguments
checkEnumType : TTImp -> Elab Name
checkEnumType ttimp = do
  Just n <- pure $ enumName ttimp
    | _ => fail "Failed to resolve type name"
  guard !(isEnumType n) $ show n ++ " is not an enumeration type."
  pure n

export
%macro
showEnum : Elab (x -> String)
showEnum = do
    Just k <- goal
      | _ => fail "dsfsd"
    logTerm "fgsfds" 1 "goal" k
    Just (IPi _ _ _ _ ty `(String)) <- goal
      | _ => fail "Required type is not: x -> String"
    n' <- checkEnumType ty
    n <- inCurrentNS (UN (nameStr n'))
    cns <- conNames n
    -- cons <- constructors n
    -- traverse (logTerm "" 1 "blah") (map snd cons)
    check `(\lam => ~(ICase eFC `(lam) ty (clause <$> cns)))
    -- check `(\lam => "sdf")
  where
    clause : Name -> Clause
    clause n = PatClause eFC (IVar eFC n) (IPrimVal eFC (Str (nameStr n)))

export
%macro
eqEnum : Elab (x -> x -> Bool)
eqEnum = do
    Just (IPi _ _ _ _ ty1 (IPi _ _ _ _ ty2 `(Prelude.Basics.Bool))) <- goal
      | _ => fail "Required type is not: x -> x -> Bool"
    n1' <- checkEnumType ty1
    n2' <- checkEnumType ty2
    n1 <- inCurrentNS (UN (nameStr n1'))
    n2 <- inCurrentNS (UN (nameStr n2'))
    guard (nameStr n1 == nameStr n2) "Required type is not: x -> x -> Bool"
    conns <- conNames n1
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
    Just (IPi _ _ _ _ ty1 (IPi _ _ _ _ ty2 `(Prelude.EqOrd.Ordering))) <- goal
      | _ => fail "Required type is not: x -> x -> Ordering"
    n1' <- checkEnumType ty1
    n2' <- checkEnumType ty2
    n1 <- inCurrentNS (UN (nameStr n1'))
    n2 <- inCurrentNS (UN (nameStr n2'))
    guard (nameStr n1 == nameStr n2) "Required type is not: x -> x -> Ordering"
    cns <- conNames n1
    clauses <- traverse clause (zip [0 .. intLength cns] cns)
    let toInt = \x => ICase eFC x `(_) clauses
    check `(\a,b => compare {ty=Int} ~(toInt `(a)) ~(toInt `(b)))
  where
    clause : (Int, Name) -> Elab Clause
    clause (i,n) = pure $ PatClause eFC (IVar eFC n) !(quote i)

||| Maps an Int value to each constructor.
||| Provide a more custom list to skip some elements. e.g.
||| enumTo ([0..12] ++ [14..30]) would skip assigning 13
export
%macro
enumTo : List Int -> Elab (x -> Int)
enumTo xs = do
    Just (IPi _ _ _ _ ty `(Int)) <- goal
      | _ => fail "Required type is not: x -> Int"
    n' <- checkEnumType ty
    n <- inCurrentNS (UN (nameStr n'))
    cons <- conNames n
    checkList xs cons
    clauses <- traverse clause (zip xs cons)
    check `(\lam => ~(ICase eFC `(lam) `(_) clauses))
  where
    clause : (a, Name) -> Elab Clause
    clause (i,n) = pure $ PatClause eFC (IVar eFC n) !(quote i)

||| Maps x's constructors to given List of Ints, resulting function yields
||| Nothing if given an Int that is not mapped.
export
%macro
enumFrom : List Int -> Elab (Int -> Maybe x)
enumFrom xs = do
    Just (IPi _ _ _ _ `(Int) `(Prelude.Types.Maybe ~(ty))) <- goal
      | _ => fail "Required type is not: Int -> Maybe x"
    n' <- checkEnumType ty
    n <- inCurrentNS (UN (nameStr n'))
    cons <- conNames n
    checkList xs cons
    clauses <- traverse clause (zip xs cons)
    let catchall_clause =  [PatClause eFC `(_) `(Nothing)]
    check `(\lam => ~(ICase eFC `(lam) `(_) (clauses ++ catchall_clause)))
  where
    clause : (a, Name) -> Elab Clause
    clause (i,n) = pure $ PatClause eFC !(quote i) `(Just ~(IVar eFC n))

||| Maps x's constructors to given List of Ints, resulting function's behavior
||| is undefined if given an Int that is not mapped.
export
%macro
unsafeEnumFrom : List Int -> Elab (Int -> x)
unsafeEnumFrom xs = do
    Just (IPi _ _ _ _ `(Int) ty) <- goal
      | _ => fail "Required type is not: Int -> x"
    n' <- checkEnumType ty
    n <- inCurrentNS (UN (nameStr n'))
    cons <- conNames n
    checkList xs cons
    clauses <- traverse clause (zip xs cons)
    check `(\lam => assert_total $ ~(ICase eFC `(lam) `(_) clauses))
  where
    clause : (a, Name) -> Elab Clause
    clause (i,n) = pure $ PatClause eFC !(quote i) (IVar eFC n)

-------------------------------------------------
-- Examples
-------------------------------------------------

-- simple enumeration type, the main target of this module
data Foo = Biz | Baz

-- indexed enumeration type
data IFoo : Nat -> Type where
  NilFoo : IFoo Z
  Bop    : IFoo (S Z)

-- more complex indexed enumeration type, don't be shocked if testing this
-- doens't work in your repl, you simply need to tell it what g or b are. This
-- would be the case if you wrote the functions by hand as well and if g or b
-- were 0-use. This is a hassle because we don't even use g or b, possibly just
-- a limitation of the type search or the fact that these instances work on
-- arguments of just Type.
data IFoo2 : Nat -> Type where
  Brap  : {g : Nat} -> IFoo2 g
  Zoppo : b `LTE` 1 => IFoo2 b
  Boppo : IFoo2 (S Z)

private
interface ToCode' a where
  toCode : a -> Int

private
interface FromCode' a where
  fromCode : Int -> Maybe a
  unsafeFromCode : Int -> a

private
Show Foo where
  show = showEnum

private
Show (IFoo n) where
  show = showEnum

private
Show (IFoo2 n) where
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

private
FromCode' Foo where
  unsafeFromCode = unsafeEnumFrom [0,1]
  fromCode = enumFrom [0,1]

eqTest1 : Baz == Baz = True
eqTest1 = Refl

eqTest2 : Baz == Biz = False
eqTest2 = Refl

enumToTest1 : toCode Baz == 1 = True
enumToTest1 = Refl

enumFromTest1 : unsafeFromCode 0 == Biz = True
enumFromTest1 = Refl

enumFromTest2 : unsafeFromCode (-12345) == Baz = True
enumFromTest2 = ?crash

enumFromSafeTest1 : fromCode 0 == Just Biz = True
enumFromSafeTest1 = Refl

enumFromSafeTest2 : fromCode 3 == Nothing {ty=Foo} = True
enumFromSafeTest2 = Refl

showTest1 : show Baz == "Baz" = True
showTest1 = Refl

showTest2 : show Bop == "Bop" = True
showTest2 = Refl

showTest3 : show (Brap {g=2}) == "Brap" = True
showTest3 = Refl

compareTest1 : Biz > Baz = False
compareTest1 = Refl

compareTest2 : Biz < Baz = True
compareTest2 = Refl

compareTest3 : compare Biz Baz = LT
compareTest3 = Refl
