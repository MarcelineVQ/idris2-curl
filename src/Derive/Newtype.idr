module Derive.Newtype

import public Derive.Common
%language ElabReflection

-- Derivaton of newtypes, datatypes with a single constructor and single field.
-- Currently just supporting simple types without parameteres and indexes.

-- Some common typeclass methods are provided as well. Technically newtype could
-- derive any typeclass its constituent has but that is a little more involved.

newtypeName : TTImp -> Maybe Name
newtypeName (IVar _ n) = Just n
newtypeName (IApp _ (IVar _ n) r) = Just n
newtypeName _ = Nothing

isNewtype : Name -> Elab Bool
isNewtype n0 = do
    [(ncon,ty)] <- constructors n0
      | _ => pure False
    pure $ fieldCount ty == 1
  where
    fieldCount : TTImp -> Int
    fieldCount (IPi _ _ ExplicitArg _ _ retty) = 1 + fieldCount retty
    fieldCount (IPi _ _ _ _ _ retty) = fieldCount retty
    fieldCount _ = 0

-- combine our checks into one spot:
-- 1. is a given TTImp possibly representing a type
-- 2. does it have exactly one constructor and one explicit field
||| Given a TTImp name it possibly a Name of some newtype and the single data
||| constructor of that newtype.
checkNewtype : TTImp -> Elab (Name, (Name, TTImp))
checkNewtype ttimp = do
  Just typename <- pure $ newtypeName ttimp
    | _ => fail "Failed to resolve type name"
  guard !(isNewtype typename) $ show typename ++ " is not a newtype."
  [onlyconstructor] <- constructors typename
    | _ => fail "Error in checkNewtype"
  pure (typename,onlyconstructor)

-------------------------------------------------

-- We use TTImp instead of Name because in name quotation on primtypes `{{Int}}
-- is not parsed as a name, in fact prims are specifically excluded from this.
||| Generate a very basic newtype, no params just wrapping.
||| newtype "Foo" `(Int) ~~> data Foo : Type where
|||                            MkFoo : Int -> Foo
export
newtype : String -> Visibility -> TTImp -> Elab ()
newtype name0 vis ty = do
    name <- inCurrentNS (UN name0)
    let con = MkTy eFC (mapName ("Mk" ++) name) `(~ty -> ~(IVar eFC name))
    let decl = IData eFC vis $ MkData eFC name (IType eFC) [] [con]
    declare [decl]
-- e.g `[  ~foo : Type where
--           ~mkfoo : ~ty -> ~foo ]

-------------------------------------------------

export
%macro
showNewtype : Elab (x -> String)
showNewtype = do
    Just (IPi _ _ _ _ ty1 `(String)) <- goal
      | _ => fail "Required type is not: x -> String"
    (n1,(conname,_)) <- checkNewtype ty1
    let con = IVar eFC conname
    check `(\a => case a of ~con x => ~(IPrimVal eFC (Str (nameStr conname)))
                                          ++ " " ++ show x)

export
%macro
eqNewtype : Elab (x -> x -> Bool)
eqNewtype = do
    Just (IPi _ _ _ _ ty1 (IPi _ _ _ _ ty2 `(Prelude.Basics.Bool))) <- goal
      | _ => fail "Required type is not: x -> x -> Bool"
    (n1,(conname,_)) <- checkNewtype ty1
    (n2,_) <- checkNewtype ty2
    let con = IVar eFC conname
    guard (nameStr n1 == nameStr n2) "Required type is not: x -> x -> Bool"
    check `(\a,b => case a of ~con x => case b of ~con y => x == y)

export
%macro
ordNewtype : Elab (x -> x -> Ordering)
ordNewtype = do
    Just (IPi _ _ _ _ ty1 (IPi _ _ _ _ ty2 `(Prelude.EqOrd.Ordering))) <- goal
      | _ => fail "Required type is not: x -> x -> Ordering"
    (n1,(conname,_)) <- checkNewtype ty1
    (n2,_) <- checkNewtype ty2
    let con = IVar eFC conname
    guard (nameStr n1 == nameStr n2) "Required type is not: x -> x -> Ordering"
    check `(\a,b => case a of ~con x => case b of ~con y => compare x y)

data FooPtr : Type where

data Fraf : Nat -> Type where
  Geb : Fraf Z

%runElab newtype "Foo1" Private `(Int) -- is a primtype
%runElab newtype "Foo2" Private `(FooPtr) -- has no cons
-- %runElab newtype "Foo3" Private `(Fraf) -- needs params, not handled yet

caseFoo1 : Foo1 -> Int
caseFoo1 (MkFoo1 x) = x

caseFoo2 : Foo2 -> FooPtr
caseFoo2 (MkFoo2 x) = x

Show Foo1 where
  show = showNewtype

Eq Foo1 where
  (==) = eqNewtype

Ord Foo1 where
  compare = ordNewtype

showTest1 : show (MkFoo1 3) == "MkFoo1 3" = True
showTest1 = ?good -- append is keeping this from reducing, but it checks out

eqTest1 : MkFoo1 3 == MkFoo1 3 = True
eqTest1 = Refl

ordTest1 : MkFoo1 3 > MkFoo1 3 = False
ordTest1 = Refl
