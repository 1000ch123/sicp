module Church(
    zero,
)where

-- ex2-06
-- チャーチ数
-- ucfsmipでやったやつや

-- 数n:関数fを引数xにn回適用する高階関数
-- 型シノニムはaliasみたいなものなので解禁します

type ChurchNum = (Int->Int) -> Int -> Int

zero :: ChurchNum
zero f = id

-- nに1増やす = xに対しfをn回適用し，さらにもう一度fを適用する
addOne :: ChurchNum -> ChurchNum
addOne n f x = f $ n f x

-- ここからex2-06
one :: ChurchNum
one f = f

two :: ChurchNum
two f = f . f

main = do
    print $ zero (+1) 0
    print $ addOne zero (+1) 0
    print $ one (+1) 0
    print $ two (+1) 0
