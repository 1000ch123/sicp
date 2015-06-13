-- ex1-39
-- tanを求める
-- ランベルトの式
import ContFrac

d :: Int -> Float
d k = fromIntegral k * 2 - 1

n :: Float -> Int -> Float
n x 1 = x
n x _ = - x ^ 2

tanCf :: Float -> Int -> Float
tanCf x = contFrac (n x) d

main = do
    print $ tan 1
    print $ tanCf 1 100

