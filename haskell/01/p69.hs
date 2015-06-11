-- p69
-- 半区間法

search :: (Float -> Float ) -> Float -> Float -> Float
search f left right
    | closeEnough left right    = mid
    | f mid > 0                 = search f left mid
    | f mid < 0                 = search f mid right
    | otherwise                 = mid
    where mid = (left + right) / 2.0

closeEnough :: Float -> Float -> Bool
closeEnough x y = abs(x - y) < 0.001

halfIntervalMethod :: (Float -> Float) -> Float -> Float -> Float
halfIntervalMethod f a b
    | f a < 0 && f b > 0    = search f a b
    | f a > 0 && f b < 0    = search f b a
    | otherwise             = 0

main = do
    print $ closeEnough 1.0001 1.0
    print $ halfIntervalMethod sin 2.0 4.0
    print $ halfIntervalMethod (\x -> x^3 - 2*x - 3.0) 1.0 2.0
