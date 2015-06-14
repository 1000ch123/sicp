-- p75
-- 返り値としての手続き

-- 関数を引数として渡すとかそのへんの話
-- 不動点処理でいろいろ

import FixedPoint

averageDamp :: (Float -> Float) -> Float -> Float
averageDamp fn x = (fn x + x) / 2

squareRoot :: Float -> Float
squareRoot x = fixedPoint (averageDamp (\y -> x / y)) 1.0

cubeRoot :: Float -> Float
cubeRoot x = fixedPoint (averageDamp (\y -> x / (y ^ 2))) 1.0

main = do
    print $ averageDamp (^2) 10
    print $ squareRoot 5.0
    print $ cubeRoot 5.0
