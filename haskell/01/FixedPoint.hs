-- p71.不動点
--

module FixedPoint(
    fixedPoint,
)where

tolerance :: Float
tolerance = 0.000001

closeEnough :: Float -> Float -> Bool
closeEnough x y = abs(x - y) < tolerance

fixedPoint :: (Float -> Float) -> Float -> Float
fixedPoint f guess
    | closeEnough guess next = next
    | otherwise              = fixedPoint f next
    where next = f guess

main = do
    print $ fixedPoint cos 1.0
