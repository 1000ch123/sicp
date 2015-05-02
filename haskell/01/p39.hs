-- p39 両替方法の数を数える
-- 両替金額と利用可能硬化を与え，両替パターン数を返す
countExchangePatterns :: Int -> [Int] -> Int
countExchangePatterns _ []  = 0
countExchangePatterns n (x:xs)
    | n == 0    = 1
    | n < 0     = 0
    | otherwise = countExchangePatterns n xs + countExchangePatterns (n-x) (x:xs)

main = do
    print $ countExchangePatterns 10 [5, 1]
    print $ countExchangePatterns 11 [10, 5, 1]
    print $ countExchangePatterns 100 [50, 25, 10, 5, 1]
