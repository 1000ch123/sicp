import ContFrac

d :: Int -> Int -> Float
d n k
    | mod x 3 == 1  = 2.0 * fromIntegral (1 + (x `div` 3))
    | otherwise     = 1.0
    where x = n - k

n :: Int -> Float
n _ = 1

euler :: Int -> Float
euler k = 2.0 + contFrac' n (d k) k

main = do
    print $ euler 100
