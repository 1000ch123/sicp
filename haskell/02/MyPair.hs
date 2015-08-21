module MyPair(
    myCons,
    myCar,
    myCdr
) where
-- ex2-05
-- 負ではない整数のペアを数値と演算命令のみで表現できることを示せ
-- hint: (a,b) => 2^a * 3^b

-- (a,b) を x = 2^a * 3^b で表現する．
-- 2,3は互いに素であるから，xを素因数分解することでa,bは一意に定まる．
-- fst(=a)を取得したければ2を何乗しているか，
-- snd(=b)を取得したければ3を何乗しているか，
-- を調べればよい
-- 2,3でなくとも，互いに素な数n,mを使えばイケそうっすね

-- というわけで，ヘルパー関数として
-- x,基数を与えられた時に指数を返す関数をつくろう

-- | countExp
-- >>> countExp 12 2
-- 2
-- >>> countExp 12 3
-- 1
-- >>> countExp 12 5
-- 0
countExp :: Int -> Int -> Int
countExp n base
    | mod n base == 0   = 1 + countExp (div n base) base
    | otherwise         = 0

-- |myCons
-- >>> myCons 1 1
-- 6
-- >>> myCons 2 3
-- 108
myCons :: Int -> Int -> Int
myCons a b = 2 ^ a * 3 ^ b

-- | myCar
-- >>> myCar 6
-- 1
-- >>> myCar 108
-- 2
myCar :: Int -> Int
myCar n = countExp n 2

-- | myCdr
-- >>> myCdr 6
-- 1
-- >>> myCdr 108
-- 3
myCdr :: Int -> Int
myCdr n = countExp n 3

