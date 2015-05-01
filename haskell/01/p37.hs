-- 線形再帰プロセス
-- どんどん深くなっていくイメージ
-- 同じ計算を何度か繰り返す可能性あり
fibRecursive :: Int -> Int
fibRecursive x
    | x == 0    = 0
    | x == 1    = 1
    | otherwise = fibRecursive (x-1) + fibRecursive (x-2)

-- 線形反復プロセス
-- 毎度評価されていくイメージ
-- 途中ステップのデータを保存しておけば再利用できる
fibRepitition :: Int -> Int
fibRepitition = fibRepititionStep 1 0


fibRepititionStep :: Int -> Int -> Int -> Int
fibRepititionStep sum step count
    | count == 0    = step
    | otherwise     = fibRepititionStep (sum + step) sum (count-1)

main = do
    print $ fibRecursive 6
    print $ fibRepitition 6
