-- p1

scaleList :: Num a => [a] -> a -> [a]
scaleList [x] n = [n * x]
scaleList (x:xs) n = n * x : scaleList xs n

map' :: (a -> b) -> [a] -> [b]
map' p [x] = [p x]
map' p (x:xs) = p x : map' p xs

main = do
    print $ scaleList [1,2,3,4,5] 10
    print $ map' (* 10) [1,2,3,4,5]
