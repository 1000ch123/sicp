-- 1-45
-- x^n = a を解く

-- 平均減衰を伴う不動点計算
--

import MyTypes
import FixedPoint
import FnRepeat

average :: Double -> Double -> Double
average a b = (a + b) / 2

averageDamp :: Transform
averageDamp fn x = average (fn x) x

nAverage :: Int -> Double -> Double -> Double
nAverage n a = repeatN n (average a)

nRoot :: Int -> Int -> Double -> Double
nRoot n k a = fixedPoint (repeatN k (averageDamp (\x -> a / (x ^ (n-1))))) 1.0
{-nRoot n k a = fixedPoint (nAverage k a0) 1.0-}
    {-where a0 x = a / (x ^ (n-1))-}

main = do
    print $ nAverage 1 0 10
    print $ repeatN 1 (averageDamp (\x -> 10 - x*x)) 4
    print $ nRoot 4 2 16.0
