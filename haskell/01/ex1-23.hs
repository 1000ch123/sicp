-- ex1-21
-- smallest-diviserの実装
import Data.Time

smallestDiviser :: (Integral a) => a -> a
smallestDiviser a = findDiviser (+ 1) a 2

findDiviser :: (Integral a) => (a->a) -> a -> a -> a
findDiviser fn x d
    | d*d > x           = x
    | mod x d == 0      = d
    | otherwise         = findDiviser fn x (fn d)

isPrime :: (Integral a) => a -> Bool
isPrime x = (==) (smallestDiviser x) x

-- ex1-22
-- 連続する奇数から素数を見つけよう
-- smallestDiviser はO(√n)で時間が増えていくか確認せよ
-- n = 1000 -> 10000 で *10
-- sqrt 10 = 3.162.. 倍の時間がかかるはず
searchForPrimes :: (Integral a) => a -> [a]
searchForPrimes n = filter isPrime $ filter odd [n..]

calcFuncTimes :: (Show a) => a -> IO()
calcFuncTimes str = do
    x <- getCurrentTime
    print str
    y <- getCurrentTime
    print $ diffUTCTime y x

-- ex1-23
-- smallestDiviserのアルゴリズム改変
-- findDiviserにおいてdは[2,3,4,5..]ではなく[2,3,5,7,...]と与えてよい.
-- dが偶数でのステップが減る分，実行時間はどう変化するか?

smallestDiviser' :: (Integral a) => a -> a
smallestDiviser' a = findDiviser next a 2

next :: (Integral a) => a -> a
next 2 = 3
next x = x+2

isPrime' :: (Integral a) => a -> Bool
isPrime' x = (==) (smallestDiviser' x) x

primes = [1009,1013,1019,10007,10009,10037,100003,100019,100043,1000003,1000033,1000037]

main = do
    calcFuncTimes $ map isPrime  primes -- ca.0.008
    calcFuncTimes $ map isPrime' primes -- ca.0.006
