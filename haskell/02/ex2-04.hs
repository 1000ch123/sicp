-- myCons:x,yを引数とし，「引数mにx,yを適用する高階関数」を返す
myCons :: a -> a-> (a -> a-> a) -> a
myCons x y m = m x y

-- myCar:高階関数zに「p,qを引数としpを返す関数 = const」を適用する
-- myCar z = z (\p q -> p)
myCar :: ((a -> a -> a) -> a) -> a
myCar z = z const

-- myCar:高階関数zに「p,qを引数としqを返す関数」を適用する
myCdr :: ((a -> a -> a) -> a) -> a
myCdr z = z (\p q -> q)

main = do
    print $ myCar $ myCons 1 2
    print $ myCdr $ myCons 1 2
