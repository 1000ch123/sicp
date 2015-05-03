exptRecursive :: Int -> Int -> Int
exptRecursive _ 0 = 1
exptRecursive b n = b * exptRecursive b (n-1)

exptRepetitive :: Int -> Int -> Int
exptRepetitive b n = exptRepetitiveIter b n 1

exptRepetitiveIter :: Int -> Int -> Int -> Int
exptRepetitiveIter base count prod
    | count == 0    = prod
    | otherwise     = exptRepetitiveIter base (count - 1) (prod * base)

fastExpt :: (Integral a) => a -> a -> a
fastExpt b n
    | n == 0    = 1
    | even n    = square $ fastExpt b (n `div` 2)
    | otherwise = b * fastExpt b (n-1)

square :: (Num a) => a -> a
square x =  x * x

main = do
    print $ exptRecursive 3 2
    print $ exptRepetitive 4 3
    print $ fastExpt 5 4
