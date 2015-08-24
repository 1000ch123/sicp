-- ex2-21
-- 引数リストの各値を２乗して返すsquare-listを２通り定義せよ

squareList1 :: Floating a => [a] -> [a]
squareList1 [x] = [x * x]
squareList1 (x:xs) = x * x : squareList1 xs

squareList2 :: Floating a => [a] -> [a]
squareList2 = map (** 2)

main = do
    print $ squareList1 [1,2,3,4,5]
    print $ squareList2 [1,2,3,4,5]
