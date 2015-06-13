-- ex1-38
-- オイラー数を求める
-- e = 2.718..
import ContFrac

d :: Int -> Float
d k
    | mod k 3 == 2  = 2.0 * fromIntegral (1 + (k `div` 3))
    | otherwise     = 1.0

n :: Int -> Float
n = const 1

euler :: Int -> Float
euler k = 2.0 + contFrac n d k

main = do
    print $ euler 100
