-- ex1-24
-- フェルマーテストで各素数をテスト
-- かかる時間を確かめよう
import System.Random
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
    gen <- getStdGen
    print $ "target:" ++ show target
    print $ take 10 $ randomRs (2,target) gen
    print $ fermatTest target (take 10 $ randomRs (2,target) gen)

calcIOTimes :: IO() -> IO()
calcIOTimes action = do
    x <- getCurrentTime
    action
    y <- getCurrentTime
    print $ diffUTCTime y x

-- フェルマーテスト
-- fastPrimeのexpModはO(log n)とかんがえられる
-- 判定対象:1000付近,1000000付近で実行時間がどう変わるだろうか？
main = do
    calcIOTimes (testAction 1009)
    calcIOTimes (testAction 1009)
    calcIOTimes (testAction 10007)
    calcIOTimes (testAction 100003)
    calcIOTimes (testAction 1000003)

-- 動作結果
--"target:1009"
-- True
-- 0.000621s
-- "target:10007"
-- True
-- 0.000629s
-- "target:100003"
-- True
-- 0.000804s
-- "target:1000003"
-- True
-- 0.000819s
--
-- あんま変わんないな？
-- コードが間違っているのか.正常動作なのか.
-- キャッシュとか効いてる?
