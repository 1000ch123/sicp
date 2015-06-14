-- p71.不動点
--

module FixedPoint(
    fixedPoint,
)where

tolerance :: Double
tolerance = 0.000001

closeEnough :: Double -> Double -> Bool
closeEnough x y = abs(x - y) < tolerance

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f guess
    | closeEnough guess next = next
    | otherwise              = fixedPoint f next
    where next = f guess

main = do
    print $ fixedPoint cos 1.0
