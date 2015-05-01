ackermann :: Int -> Int ->Int
ackermann x y
    | y == 0     = 0
    | x == 0     = 2 * y
    | y == 1     = 2
    | otherwise  = ackermann (x-1) (ackermann x (y-1))

main = do
    print $ ackermann 1 10
    print $ ackermann 2 4
    print $ ackermann 3 3
