cube :: (Num a)=> a -> a
cube x = x * x * x

p :: (Num a) => a -> a
p x = 3 * x - 4 * cube x

sine :: (Fractional a, Ord a) => a -> a
sine angle
    | abs angle < 0.1   = angle
    | otherwise         = p . sine $ angle / 3.0


main = do
    print $ sine (pi/6)
    print $ sine (pi/2)
    where pi = 3.14159265359

-- a
-- sine 12.15 を評価時，pは何回適用されるか？


-- b
-- sine a を評価時，sine手続きによる生成プロセスの
-- 記憶域，ステップ数の増加オーダーをaの関数で表わせ
