module Derive.Newtype

import Language.Reflection
%language ElabReflection

-- Derivaton of newtypes, datatypes with a single constructor and single field.
-- Currently just supporting simple types without parameteres and indexes.

-------------------------------------------------
-- Helpers, moving these to another module breaks Elaboration :S
-------------------------------------------------

eFC : FC
eFC = EmptyFC

mapName : (String -> String) -> Name -> Name
mapName f (UN x) = UN (f x)
mapName f (MN x y) = MN (f x) y
mapName f (NS x y) = NS x (mapName f y)
mapName f (DN x y) = DN (f x) y

-------------------------------------------------

-- We use TTImp instead of Name because in name quotation on primtypes `{{Int}}
-- is not parsed as a name, in fact prims are specifically excluded from this.
||| Very basic newtypes, no params just wrapping.
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
