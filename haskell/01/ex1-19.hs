-- ex1-19
-- fibを対数ステップ数で解くアルゴリズム

-- イメージ
-- T_pq:(a,b) -> (a',b')
-- S: T_pq^2 -> T_p'q'

-- Tはfibを1ステップ進める操作
-- SはT2回分をT1回分に変換する操作
-- fib(n)は(1,0)にTをn回作用させることに等しい
-- T^nの計算をfastExptのように効率化する

fib :: (Integral a) => a -> a
fib = fibIter 1 0 0 1

fibIter :: (Integral a) => a -> a -> a -> a -> a -> a
fibIter a b p q count
    | count == 0    = b
    | even count    = fibIter a b (p^2 + q^2) (q^2 + 2*p*q) (div count 2)
    | otherwise     = fibIter ((p+q)*a + q*b) (q*a + p*b) p q (count -1)

main = do
    print $ fib 6
    print $ fib 10
