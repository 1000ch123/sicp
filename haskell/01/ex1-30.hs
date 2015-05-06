-- ex1-30

sumInt :: Int -> Int -> Int
sumInt a b
    | a > b     = 0
    | otherwise = a + sumInt (a+1) b

sumCubes :: Int -> Int -> Int
sumCubes a b
    | a > b     = 0
    | otherwise = cube a + sumCubes (a+1) b

cube :: (Num a) => a -> a
cube x = x ^ 3

sumPi :: Float -> Float -> Float
sumPi a b
    | a > b     = 0
    | otherwise = step a + sumPi (a+4) b

step :: Float -> Float
step a = 1 / (a * (a + 2))

-- 全てこのパターンでかけるよ！
sum' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
sum' fn nx a b
    | a > b     = 0
    | otherwise = fn a + sum' fn nx (nx a) b

-- 反復的にsum定義
-- sum''のスコープ内でsumIterを定義したいがどうすれば？
-- whereとかlet?
sum'' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
sum'' fn nx a b = sumIter fn nx a b 0

sumIter :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a -> a
sumIter fn nx a b result
    | a > b     = result
    | otherwise = sumIter fn nx (nx a) b (result + fn a)

-- 「ある関数を指定回数適用する」関数を返す関数
rep :: (Num a, Ord a) => (a->a) -> a -> a -> a
rep f 0 = id
rep f n = f . rep f (n-1)

-- sumを応用して積分
-- 積分 = 面積(fn)の足し算(sum) とかんがえる
-- 範囲[a,b]を幅dxの長方形とみなす
-- 長方形は [a ,a + dx]...の範囲となるので，高さは代表としてf(a + dx/2)を用いる
integral' :: (Num a, Ord a, Floating a) => (a->a) -> a -> a -> a -> a
integral' fn a b dx = sum' ((*dx).fn) (+dx) (a + dx / 2.0) b

main = do
    print $ sumInt 0 5
    print $ sumCubes 0 5
    print $ 8 * sumPi 1 10

    print $ sum' id succ 0 5
    print $ sum' cube succ 0 5
    print $ 8 * sum' step (rep succ 4) 1 10
    print $ integral' cube 0 1 0.01

    print $ sum'' cube succ 0 5
