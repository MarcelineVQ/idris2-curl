module Util

import Data.List

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
