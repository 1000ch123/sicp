-- P68

f :: (Num a) => (a->a) -> a
f g = g 2

main = do
    print $ f sqrt
    print $ f (\x -> x * (x + 1))

-- f f
-- f 2
-- 2 2
-- 定数を定数に適用しようとするんだろうなぁ
