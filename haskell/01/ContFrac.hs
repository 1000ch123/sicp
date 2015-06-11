module ContFrac(
    contFrac,
    contFracReptitive
)where

contFrac :: Float -> Float -> Int -> Float
contFrac _ _ 0 = 1
contFrac n d k = n / (d + contFrac n d (k-1))

contFracReptitive :: Int -> Float
contFracReptitive cnt = contFracReptitiveStep 1 1 1 cnt

contFracReptitiveStep :: Float -> Float -> Float -> Int -> Float
contFracReptitiveStep v n d k
    | k == 0    = v
    | otherwise = contFracReptitiveStep (n / (d + v)) n d (k-1)


