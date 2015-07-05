module Fraction(
    makeRat,
)where

-- |makeRat
-- >>> makeRat 1 2
-- (1.0,2.0)
makeRat :: Float -> Float -> (Float, Float)
makeRat a b = (a,b)

-- |numer
-- >>> let a = makeRat 2 3
-- >>> numer a
-- 2.0
numer :: (Float,Float) -> Float
numer = fst

-- |denom
-- >>> let a = makeRat 2 3
-- >>> denom a
-- 3.0
denom :: (Float, Float) -> Float
denom = snd

-- |printRat
-- >>> let a = makeRat 2 3
-- >>> printRat a
-- "2.0/3.0"
printRat :: (Show a) => (a,a) -> IO()
printRat (a,b) = print $ show a ++ "/" ++ show b
