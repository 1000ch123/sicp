module ContFrac(
    contFrac,
    {-contFrac',-}
    contFracReptitive,
)where

{-contFrac :: Float -> Float -> Int -> Float-}
{-contFrac _ _ 0 = 1-}
{-contFrac n d k = n / (d + contFrac n d (k-1))-}

contFrac :: (Int -> Float) -> (Int -> Float) -> Int -> Float
contFrac fn fd k = contFrac' fv fn fd k
    where fv = argReverse k

contFrac' :: (Int -> Int) -> (Int -> Float) -> (Int -> Float) -> Int -> Float
contFrac' _ _ _ 0 = 1
contFrac' fv fn fd k = fn k' / (fd k' + contFrac' fv fn fd (k-1))
    where k' = fv k

-- 1,2,..,n -> n,n-1,...,1
argReverse :: Int -> Int -> Int
argReverse n x = n - x + 1

contFracReptitive :: Int -> Float
contFracReptitive = contFracReptitiveStep 1 1 1

contFracReptitiveStep :: Float -> Float -> Float -> Int -> Float
contFracReptitiveStep v n d k
    | k == 0    = v
    | otherwise = contFracReptitiveStep (n / (d + v)) n d (k-1)



main = do
    print $ contFrac (const 1) (const 1) 100
