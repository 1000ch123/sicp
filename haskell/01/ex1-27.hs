-- ex1-27
-- フェルマーテスト with カーマイケル数
-- target未満の数値すべてにtestするversion

import Data.Time

expmod :: (Integral a) => a -> a -> a -> a
expmod base exp m
    | exp == 0  = 1
    | even exp  = (`mod` m) . sq $ expmod base (div exp 2) m
    | otherwise = (`mod` m) . (*base) $ expmod base (exp - 1) m

sq :: (Integral a) => a -> a
sq a = a * a

fermatTest :: (Integral a) => a -> [a] -> Bool
fermatTest target = all (fastPrime target)

fastPrime :: (Integral a) => a -> a -> Bool
fastPrime n a = expmod a n n == a

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
    calcIOTimes (testAction 109)
    calcIOTimes (testAction 561) -- 561 = 3 * 11 * 17
    calcIOTimes (testAction 1105)

