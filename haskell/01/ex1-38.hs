-- ex1-38
-- オイラー数を求める
-- e = 2.718..
import ContFrac

d :: Int -> Double
d k
    | mod k 3 == 2  = 2.0 * fromIntegral (1 + (k `div` 3))
    | otherwise     = 1.0

n :: Int -> Double
n = const 1

euler :: Int -> Double
euler k = 2.0 + contFrac n d k

main = do
    print $ euler 100
