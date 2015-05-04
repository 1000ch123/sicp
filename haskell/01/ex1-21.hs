-- ex1-21
-- smallest-diviserの実装

smallestDiviser :: (Integral a) => a -> a
smallestDiviser a = findDiviser a 2

findDiviser :: (Integral a) => a -> a -> a
findDiviser x d
    | d*d > x           = x
    | mod x d == 0      = d
    | otherwise         = findDiviser x (d+1)


main = do
    print $ smallestDiviser 199
    print $ smallestDiviser 1999
    print $ smallestDiviser 19999
