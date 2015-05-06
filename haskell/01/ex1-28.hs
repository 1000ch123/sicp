-- ex1-28
-- ミラー・ラビン テスト with カーマイケル数
-- target未満の数値すべてにtestするversion

import Data.Time

expmod :: (Integral a) => a -> a -> a -> a
expmod base exp m
    | exp == 0  = 1
    | even exp  = (`mod` m) . mrsq m $ expmod base (div exp 2) m
    | otherwise = (`mod` m) . (*base) $ expmod base (exp - 1) m

-- expmodの2乗判定部分をちょっと厳しくする
-- 詳しくはex1-28.jpg
mrsq :: (Integral a) => a -> a -> a
mrsq n x
    | x == 1     = 1
    | x == n-1   = 1
    | x2 == 1    = 0
    | otherwise  = x2
    where x2 = mod (x*x) n

fermatTest :: (Integral a) => a -> [a] -> Bool
fermatTest target = all (fastPrime target)

fastPrime :: (Integral a) => a -> a -> Bool
fastPrime n a = expmod a (n-1) n == 1

testAction :: Int -> IO()
testAction target = do
    print $ "target:" ++ show target
    print $ fermatTest target [2..target-1]

calcIOTimes :: IO() -> IO()
calcIOTimes action = do
    x <- getCurrentTime
    action
    y <- getCurrentTime
    print $ diffUTCTime y x

-- フェルマーテスト
-- カーマイケル数:フェルマーテストを騙してしまう数
-- 素数じゃないのにフェルマーテストがtrueとなる
main = do
    calcIOTimes (testAction 3)
    calcIOTimes (testAction 561) -- 561 = 3 * 11 * 17
    calcIOTimes (testAction 1105)

