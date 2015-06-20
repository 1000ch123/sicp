-- ex1-46
-- iterateImprove

type Check = Double -> Bool
type Update = Double -> Double
type Guess = Double
type Answer = Double

iterativeImprove :: Check -> Update -> Guess -> Answer
iterativeImprove check update guess
    | check guess   = guess
    | otherwise     = iterativeImprove check update (update guess)

-- sample:check
tolerance :: Double
tolerance = 0.000001

closeEnough :: Double -> Double -> Bool
closeEnough x y = abs(x - y) < tolerance

closeEnoughSquare :: Double -> Double -> Bool
closeEnoughSquare a x = closeEnough a (x^2)

-- sample:update
-- 平均減衰を一回分利用
next :: Double -> Double -> Double
next a x = (a / x + x) / 2

-- sample:sqrt
iterativeSqrt :: Double -> Double
iterativeSqrt a = iterativeImprove (closeEnoughSquare a) (next a) 1.0

main = do
    print $ iterativeSqrt 4.0
