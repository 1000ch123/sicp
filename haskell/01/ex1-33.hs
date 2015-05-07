-- 1-33
-- accumulate + filter

-- 再帰的定義
filteredAccumulate :: (Num a, Ord a) => (a -> a -> a) -> a -> (a->a) -> (a -> a) -> (a -> Bool) -> a -> a -> a
filteredAccumulate combiner nval fn nx fi a b
    | a > b     = nval
    | fi a      = combiner (fn a) (filteredAccumulate combiner nval fn nx fi (nx a) b)
    | otherwise = filteredAccumulate combiner nval fn nx fi (nx a) b


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


filteredSum :: (Num a, Ord a) => (a->a) -> (a->a) -> (a->Bool) -> a -> a -> a
filteredSum = filteredAccumulate (+) 0

sumPrimeSquare :: (Num a, Ord a, Integral a) => a -> a -> a
sumPrimeSquare = filteredSum (^2) (+1) isPrime


main = do
    print $ sumPrimeSquare 2 5
