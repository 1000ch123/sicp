-- ex1-31


product' :: (Num a, Ord a) => (a->a) -> (a->a) -> a -> a -> a
product' fn nx a b
    | a > b     = 1
    | otherwise = fn a * product' fn nx (nx a) b

main = do
    print $ product' (^2) (+1) 1 3
