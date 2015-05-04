-- ex1-21
-- smallest-diviserの実装
import Data.Time

smallestDiviser :: (Integral a) => a -> a
smallestDiviser a = findDiviser a 2

findDiviser :: (Integral a) => a -> a -> a
findDiviser x d
    | d*d > x           = x
    | mod x d == 0      = d
    | otherwise         = findDiviser x (d+1)

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

main = do
    calcFuncTimes $ take 3 $ searchForPrimes 100
    calcFuncTimes $ take 3 $ searchForPrimes 1000
    calcFuncTimes $ take 3 $ searchForPrimes 10000
    calcFuncTimes $ take 3 $ searchForPrimes 100000
    calcFuncTimes $ take 3 $ searchForPrimes 1000000
    calcFuncTimes $ take 3 $ searchForPrimes 10000000
