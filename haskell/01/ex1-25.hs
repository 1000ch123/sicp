-- ex1-25
-- expmodについて

-- ex1-24
expmod :: (Integral a) => a -> a -> a -> a
expmod base exp m
    | exp == 0  = 1
    | even exp  = (`mod` m) . sq $ expmod base (div exp 2) m
    | otherwise = (`mod` m) . (*base) $ expmod base (exp - 1) m

sq :: (Integral a) => a -> a
sq a = a * a

-- ex1-25
fastExpt :: Int -> Int -> Int
fastExpt base exp
    | exp == 0  = 1
    | even exp  = sq $ fastExpt base (div exp 2)
    | otherwise = (*base) $ fastExpt base (exp - 1)

expmod' :: Int -> Int -> Int -> Int
expmod' base exp m = (`mod` m) (fastExpt base exp)

expmod'' :: Int -> Int -> Int -> Int
expmod'' base exp m = (`mod` m) (fastExpt (mod base m) exp)



main = do
    print $ expmod  123 1009 1009 -- 123
    print $ expmod' 123 1009 1009 -- 466 !?
    print $ expmod'' 123 1009 1009 -- 466 !?

-- なぜこんなことが?
-- exmpod'では 123^1009を一旦評価する必要がある
-- ちょーでかい数に一度だけ a mod m を適用するイメージ
-- これは非常に大きい数となるため，オーバーフローが発生する.
-- これにより正しいexpmodが求められない.
--
-- 一方でexpmodでは合同関係を利用し，余りだけに注目している
-- a mod m を何度も適用するイメージ
-- このため評価する数字が小さく保たれるので，正しくexpmodが求められる.
--
-- fastExptのなかで余りに注目して合同値にしていけばあるいはok
