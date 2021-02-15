module Util

import Data.List
import Derive.Common

infixl 1 <&>
export
(<&>) : Functor f => f a -> (a -> b) -> f b
x <&> f = f <$> x

infixl 4 <$,$>

-- For when Lazy is causing type problems
infixr 4 &&|
export
(&&|) : Bool -> Bool -> Bool
(&&|) x y = x && y

export
catMaybes : List (Maybe a) -> List a
catMaybes z = foldr (\m,f => maybe f (\x => (x ::) . f) m) id z []

export
unzip : List (a,b) -> (List a, List b)
unzip = foldr (\(x,y),(xs,ys) => (x :: xs, y :: ys)) ([],[])

-- This is hiiiiiideously slow! maybe it's because I'm using it at elab-time
export
unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 = Data.Zippable.unzip3
-- unzip3 [] = ([],[],[])
-- unzip3 ((x, (y, z)) :: ls) = let (xs,ys,zs) = Util.unzip3 ls
--                              in (x :: xs, y:: ys, z :: zs)

export
isJust : Maybe a -> Bool
isJust (Just _ ) = True
isJust _ = False

export
unless : Applicative f => Bool -> Lazy (f ()) -> f ()
unless b act = if b then pure () else act

-- slow, preserves order
export
difference : Eq a => List a -> List a -> List a
difference xs ys = foldr (\y,xs => delete y xs) xs ys

-- TODO: bring this up with other devs for idris about it's merits and negatives
-- Intended use is to crash in places that crashing is correct to do because the
-- failure isn't recoverable anyway
export
total -- the lie
%foreign "scheme:lambda (x) (blodwen-error-quit x)"
         -- "javascript:lambda:(x)=>{throw new IdrisError(x)}"
lie_idris_crash : String -> a

export
getTTImpFC : TTImp -> FC
getTTImpFC (IVar fc _) = fc
getTTImpFC (IPi fc _ _ _ _ _) = fc
getTTImpFC (ILam fc _ _ _ _ _) = fc
getTTImpFC (ILet fc _ _ _ _ _ _) = fc
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
getTTImpFC (IAs fc _ _ _ _) = fc
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
