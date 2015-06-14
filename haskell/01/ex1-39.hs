-- ex1-39
-- tanを求める
-- ランベルトの式
import ContFrac

d :: Int -> Double
d k = fromIntegral k * 2 - 1

n :: Double -> Int -> Double
n x 1 = x
n x _ = - x ^ 2

tanCf :: Double -> Int -> Double
tanCf x = contFrac (n x) d

main = do
    print $ tan 1
    print $ tanCf 1 100

