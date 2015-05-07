-- ex1-29
-- シンプソンの公式
sum' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
sum' fn nx a b
    | a > b     = 0
    | otherwise = fn a + sum' fn nx (nx a) b

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

-- シンプソンの公式を用いてintegralを定義
integralSimpson :: (Num a, Ord a, Floating a, RealFrac a) => (a->a) -> a -> a -> a -> a
integralSimpson fn a b n = (h/3) * sum' (simpson fn a limit h) (+1) 0 limit
    where h = (b-a) / (2*n)
          limit = 2*n

simpson :: (Num a, Ord a, Floating a, RealFrac a) => (a -> a) -> a -> a -> a -> a -> a
simpson fn a limit h k = w limit k * y fn a h k

w :: (Num a, Ord a, Floating a, RealFrac a) => a -> a -> a
w b k
    | k == 0     = 1
    | k == b     = 1
    | even . truncate $ k = 2
    | odd . truncate $ k = 4

y :: (Num a, Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
y fn a h k = fn (a+k*h)

cube :: (Num a) => a -> a
cube x = x ^ 3


main = do
    print "ex1-29"
    print $ integralSimpson cube 0 1 10000
