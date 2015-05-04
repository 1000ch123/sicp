-- ex1-17
-- 加算のみ可能な状態で,乗算を定義せよ
mul :: Int -> Int -> Int
mul a b
    | b == 0    = 0
    | otherwise = a + mul a (b - 1)

-- 整数を倍にする関数 twice
-- 偶数を半分にする関数 halve
-- を使用可能とする
fastMul :: (Integral a) => a -> a -> a
fastMul a b
    | b == 0    =0
    | even b    = twice $ fastMul a (halve b)
    | otherwise = a + fastMul a (b-1)

twice :: (Num a) => a -> a
twice x = x * 2

halve :: (Integral a) => a -> a
halve x = div x 2

main = do
    print $ mul 3 1000
    print $ fastMul 3 1000
