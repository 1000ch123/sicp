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


-- |addRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> addRat a b
-- (22.0,15.0)
addRat :: (Float, Float) -> (Float, Float) -> (Float, Float)
addRat a b = makeRat (numer a * denom b + numer b * denom a) (denom a * denom b)

-- |subRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> subRat a b
-- (-2.0,15.0)
subRat :: (Float, Float) -> (Float, Float) -> (Float, Float)
subRat a b = makeRat (numer a * denom b - numer b * denom a) (denom a * denom b)

-- |mulRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> mulRat a b
-- (8.0,15.0)
mulRat :: (Float, Float) -> (Float, Float) -> (Float, Float)
mulRat a b = makeRat (numer a * numer b) (denom a * denom b)

-- |divRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> divRat a b
-- (10.0,12.0)
divRat :: (Float, Float) -> (Float, Float) -> (Float, Float)
divRat a b = makeRat (numer a * denom b) (denom a * numer b)

-- |sameRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> sameRat a b
-- False
--
-- >>> let c = makeRat 2 3
-- >>> sameRat a c
-- True
sameRat :: (Float, Float) -> (Float, Float) -> Bool
sameRat a b = numer a == numer b && denom a == denom b



