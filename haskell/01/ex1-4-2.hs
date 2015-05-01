-- シンプルにabs使う
aPlusAbsb :: Int -> Int -> Int
aPlusAbsb a b = a + abs b

-- if分岐で適切な関数返す
-- ちょっと違うけど..
absAdd2 :: Int -> Int -> Int
absAdd2 x = (+) (if (x > 0) then x else (-x))

-- ガードつかってみる
-- 一番関数っぽい気がする
absAdd3 :: Int -> Int -> Int
absAdd3 x y
    | y < 0     = x - y
    | otherwise = x + y

-- swapぽいことすればいけるのでは
-- absAdd4 :: Int -> Int -> Int

main = do
    print $ aPlusAbsb 10 (-20)
    print $ aPlusAbsb 10 20
    print $ absAdd2 10 20
    print $ absAdd2 (-10) 20
    print $ absAdd3 10 20
    print $ absAdd3 20 (-10)



