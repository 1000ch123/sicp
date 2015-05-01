-- ex1-12
-- パスカルの三角形
-- input:n段
-- output:配列
-- 線形再帰プロセス
pascalTriangle :: Int -> [Int]
pascalTriangle n
    | n == 1    = [1]
    | otherwise = zipWith (+) (0 : arr) (arr ++ [0])
    where arr = pascalTriangle (n-1)

main = do
    print $ pascalTriangle 1
    print $ pascalTriangle 3
    print $ pascalTriangle 5
    print $ pascalTriangle 9
