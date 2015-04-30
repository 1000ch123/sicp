main = do
    print $ aPlusAbsb 10 (-20)
    print $ aPlusAbsb 10 20
    where
        aPlusAbsb :: Int -> Int -> Int
        aPlusAbsb a b = a + abs b



