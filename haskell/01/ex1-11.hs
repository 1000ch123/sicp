-- ex1-11
-- 線形再帰プロセス
funcRecursive :: Int -> Int
funcRecursive 1 = 1
funcRecursive 2 = 2
funcRecursive 3 = 3
funcRecursive x = funcRecursive(x-1) + 2*funcRecursive(x-2) + 3*funcRecursive(x-3)

-- 線形反復プロセス
funcRepetitive :: Int -> Int
funcRepetitive = funcRepetitiveStep 3 2 1

funcRepetitiveStep :: Int -> Int -> Int -> Int -> Int
funcRepetitiveStep s1 s2 s3 count
    | count == 1    = s3
    | otherwise     = funcRepetitiveStep (s1 + 2*s2 + 3*s3) s1 s2 (count-1)

-- 実行!
main = do
    print $ funcRecursive 5
    print $ funcRepetitive 5
