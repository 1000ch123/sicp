module Fraction(
    makeRat,
)where

-- Ex2.1:makeRatを負数に対応
-- |makeRat
-- >>> makeRat 1 2
-- (1,2)
-- >>> makeRat (-1) 2
-- (-1,2)
-- >>> makeRat 1 (-2)
-- (-1,2)
-- >>> makeRat (-1) (-2)
-- (1,2)
makeRat :: Int -> Int -> (Int, Int)
makeRat a b
    | a >= 0 && b > 0 = makeRat' a b
    | a < 0  && b > 0 = makeRat' a b
    | a >= 0 && b < 0 = makeRat' (negate a) (negate b)
    | a < 0  && b < 0 = makeRat' (negate a) (negate b)
    | otherwise       = makeRat' a b

-- |makeRat'
-- >>> makeRat' 1 2
-- (1,2)
-- >>> makeRat' 2 4
-- (1,2)
makeRat' :: Int -> Int -> (Int, Int)
makeRat' a b = (div a g, div b g)
    where g = gcd a b
-- |numer
-- >>> let a = makeRat 2 3
-- >>> numer a
-- 2
numer :: (Int,Int) -> Int
numer = fst

-- |denom
-- >>> let a = makeRat 2 3
-- >>> denom a
-- 3
denom :: (Int, Int) -> Int
denom = snd

-- |printRat
-- >>> let a = makeRat 2 3
-- >>> printRat a
-- "2/3"
printRat :: (Show a) => (a,a) -> IO()
printRat (a,b) = print $ show a ++ "/" ++ show b


-- |addRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> addRat a b
-- (22,15)
addRat :: (Int, Int) -> (Int, Int) -> (Int, Int)
addRat a b = makeRat (numer a * denom b + numer b * denom a) (denom a * denom b)

-- |subRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> subRat a b
-- (-2,15)
subRat :: (Int, Int) -> (Int, Int) -> (Int, Int)
subRat a b = makeRat (numer a * denom b - numer b * denom a) (denom a * denom b)

-- |mulRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> mulRat a b
-- (8,15)
mulRat :: (Int, Int) -> (Int, Int) -> (Int, Int)
mulRat a b = makeRat (numer a * numer b) (denom a * denom b)

-- |divRat
-- >>> let a = makeRat 2 3
-- >>> let b = makeRat 4 5
-- >>> divRat a b
-- (5,6)
divRat :: (Int, Int) -> (Int, Int) -> (Int, Int)
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
sameRat :: (Int, Int) -> (Int, Int) -> Bool
sameRat a b = numer a == numer b && denom a == denom b



