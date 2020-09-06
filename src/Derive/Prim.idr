module Derive.Prim

import public Language.Reflection
%language ElabReflection

-- Derivations for common prim creation needs
-- Use of this module automatically imports Language.Reflection and requires
-- %language ElabReflection to be enabled below your imports.
-- Examples at module bottom

eFC : FC
eFC = EmptyFC

toIVar : Name -> TTImp
toIVar = IVar eFC

toIBindVar : String -> TTImp
toIBindVar = IBindVar eFC

argCount : TTImp -> Nat
argCount (IPi _ _ _ _ _ retty) = S (argCount retty)
argCount retty = Z

-- makeHasIO "what" Private ["c:foo","javascript:borp"]
--                         `[ prim_gob : Int -> Int -> PrimIO Int ]
-- This generates:
-- %foreign "c:foo" "javascript:borp"
-- prim_gob : Int -> Int -> PrimIO Int
-- And:
-- private
-- what : HasIO io => Int -> Int -> io Int
-- what x y = primIO $ prim_gob x y
-- This is not as useful as is it looks since often you'll be wanting to supply
-- specific arguments to your `prim_gob` or pattern match on `what`'s arguments,
-- this doesn't do that and is a direct translation/application.
-- Still it can alleviate some of the write-twice burden PrimIO can introduce.
export
makeHasIO : String -> Visibility -> List String -> List Decl -> Elab ()
makeHasIO funname0 vis str ys = do
    [IClaim pfc pmw pvis _ pty@(MkTy tyfc primname primty)] <- pure ys
      | _ => fail "bad format"
    let funty = IPi eFC MW AutoImplicit Nothing
               `(HasIO ~(IBindVar eFC "io"))
               (spiderType primty)
        funname = UN funname0
        primclaim = IClaim pfc pmw pvis [ForeignFn str] pty
        funclaim = IClaim eFC pmw vis [] (MkTy tyfc funname funty)
        varnames = map (("var" ++) . show) [0..]
        lvars = IBindVar eFC <$> take (argCount primty) varnames
        rvars = toIVar . UN <$> take (argCount primty) varnames
        lhs = foldl (\xs,x => `(~xs ~x)) (toIVar funname) lvars
        rhs = `(primIO ~(foldl (\xs,x => `(~xs ~x)) (toIVar primname) rvars))
        patclause = PatClause eFC lhs rhs
        funbody = IDef eFC funname [patclause]
    declare [primclaim] -- declare the primitive as-given
    declare [funclaim, funbody] -- declare our HasIO version
    pure ()
  where
    spiderType : TTImp -> TTImp -- replace 'PrimIO' with 'io'
    spiderType (IPi a b c d e retty) = IPi a b c d e (spiderType retty)
    spiderType (IApp _ `(PrimIO) retty) = `(~(toIBindVar "io") ~retty)
    spiderType ty = ty

%runElab makeHasIO "what" Private ["C:div,libc,math.h"]
                                 `[ prim_gob : Int -> Int -> PrimIO Int ] --`

gob_exists : PrimIO Int
gob_exists = prim_gob 1 2

what_exists : HasIO io => io Int
what_exists = what 1 2
