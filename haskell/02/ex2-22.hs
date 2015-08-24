-- ex2-22
-- square-listを反復プロセスで定義するよ

squareList :: Floating a => [a] -> [a]
squareList xs = step2 xs []


step1 :: Floating a => [a] -> [a] -> [a]
step1 [] acc = acc
step1 (x:xs) acc = step1 xs (x ** 2 : acc)
-- stepはaccに1,2..の順でconsしていく
-- 故に
-- acc = []
-- acc = f(1) : []
-- acc = f(2) : []
-- acc = f(3) : []
-- ..
-- となり，最終的に
-- acc = [f(n),f(n-1),...,f(2),f(1)]
-- が出力される

step2 :: Floating a => [a] -> [a] -> [a]
step2 [] acc = acc
step2 (x:xs) acc = step2 xs (acc ++ [x ** 2])
-- これならうまくいきそう
-- acc : x ** 2 は型が違うのでダメ
-- schemeでこれやると，ある配列の先頭要素として配列を追加することになり，
-- 出力は入れ子の配列となる．のでダメ

main = do
    print $ squareList [1,2,3,4,5]
