-- 1-32
-- accumulate

-- 再帰的定義
accumulate :: (Num a, Ord a) => (a -> a -> a) -> a -> (a->a) -> (a->a) -> a -> a -> a
accumulate combiner nval fn nx a b
    | a > b     = nval
    | otherwise = combiner (fn a) (accumulate combiner nval fn nx (nx a) b)

-- 反復的定義
accumulate' :: (Num a, Ord a) => (a -> a -> a) -> a -> (a->a) -> (a->a) -> a -> a -> a
accumulate' combiner nval fn nx a b = iter a nval
    where
        iter a acc
            | a > b     = acc
            | otherwise = iter (nx a) (combiner (fn a) acc)

sum' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
sum' = accumulate (+) 0

prod' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
prod' = accumulate (*) 1

sum'' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
sum'' = accumulate' (+) 0

prod'' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
prod'' = accumulate' (*) 1

main = do
    print $ accumulate (+) 0 (^2) (+1) 1 5
    print $ sum' (*1) (+1) 1 5
    print $ prod' (*1) (+1) 1 5
    print $ sum'' (*1) (+1) 1 5
    print $ prod'' (*1) (+1) 1 5
