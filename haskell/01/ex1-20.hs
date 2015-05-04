-- ex1_20
--

gcd' :: (Integral a) => a -> a -> a
gcd' a b
    | b == 0    = a
    | otherwise = gcd' b (mod a b)

main = do
    print $ gcd' 26 4
    print $ gcd' 206 40

-- 評価順はどうなっているのか
-- lispは適応準評価

-- 適応順:細かいところから先に評価
-- 正規順:外側から評価

-- 適応順と正規順で，最後の一回が呼ばれるかどうかが違う
-- 正規順では4回
-- 適応順では5回.最後の1回はmod 4 0となりゼロ除算
