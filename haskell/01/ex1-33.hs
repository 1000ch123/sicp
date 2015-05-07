-- 1-33
-- accumulate + filter

-- 再帰的定義
filteredAccumulate :: (Num a, Ord a) => (a -> a -> a) -> a -> (a->a) -> (a -> a) -> (a -> Bool) -> a -> a -> a
filteredAccumulate combiner nval fn nx fi a b
    | a > b     = nval
    | fi a      = combiner (fn a) (filteredAccumulate combiner nval fn nx fi (nx a) b)
    | otherwise = filteredAccumulate combiner nval fn nx fi (nx a) b

filteredSum :: (Num a, Ord a) => (a->a) -> (a->a) -> (a->Bool) -> a -> a -> a
filteredSum = filteredAccumulate (+) 0

filteredProd :: (Num a, Ord a) => (a->a) -> (a->a) -> (a->Bool) -> a -> a -> a
filteredProd = filteredAccumulate (*) 1

-- 素数判定
isPrime :: (Integral a) => a -> Bool
isPrime a = fermatTest a [1..a-1]

fermatTest :: (Integral a) => a -> [a] -> Bool
fermatTest target = all (fastPrime target)

fastPrime :: (Integral a) => a -> a -> Bool
fastPrime n a = expmod a (n-1) n == 1

expmod :: (Integral a) => a -> a -> a -> a
expmod base exp m
    | exp == 0  = 1
    | even exp  = (`mod` m) . mrsq m $ expmod base (div exp 2) m
    | otherwise = (`mod` m) . (*base) $ expmod base (exp - 1) m

mrsq :: (Integral a) => a -> a -> a
mrsq n x
    | x == 1     = 1
    | x == n-1   = 1
    | x2 == 1    = 0
    | otherwise  = x2
    where x2 = mod (x*x) n

-- 素数二乗和
sumPrimeSquare :: (Num a, Ord a, Integral a) => a -> a -> a
sumPrimeSquare = filteredSum (^2) (+1) isPrime

-- GCD求める関数
gcd' :: (Integral a) => a -> a -> a
gcd' a b
    | b == 0    = a
    | otherwise = gcd' b (mod a b)

isNPrime :: (Integral a) => a -> a -> Bool
isNPrime a b = (gcd' a b == 1)

prodNPrime :: (Num a, Ord a, Integral a) => a -> a
prodNPrime a = filteredProd (id) (+1) (isNPrime a) 1 a


main = do
    print $ sumPrimeSquare 2 5
    print $ prodNPrime 7
