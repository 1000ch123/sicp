-- ex1-16
-- fast-exptを反復プロセスとして定義せよ
-- 反復:状態値さえ与えればどこからでも再開できる
-- 状態の「言い換え」を繰り返すイメージ
fastExptRepetitive :: Int -> Int -> Int
fastExptRepetitive base count = fastExptRepetitiveStep base count 1

-- b0       = 1
-- b^(2n)   = (b^2) ^ n
-- b^(2n+1) = b * b^(2n)
fastExptRepetitiveStep :: (Integral a) => a -> a -> a -> a
fastExptRepetitiveStep base count prod
    | count == 0    = prod
    | even count    = fastExptRepetitiveStep (base * base) (count `div` 2) prod
    | otherwise     = fastExptRepetitiveStep base (count -1) prod*base

main = do
    print $ fastExptRepetitive 5 3
    print $ fastExptRepetitive 3 7
