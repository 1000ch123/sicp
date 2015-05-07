-- ex1-31

-- 再帰プロセス
product' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
product' fn nx a b
    | a > b     = 1
    | otherwise = fn a * product' fn nx (nx a) b

-- 反復プロセス
product'' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
product'' fn nx a b = iter a 1
    where
        iter a res
            | a > b     = res
            | otherwise = iter (nx a) (fn a * res)

main = do
    print $ product'  (^2) (+1) 1 3
    print $ product'' (^2) (+1) 1 3

