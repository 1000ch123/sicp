cube :: (Num a)=> a -> a
cube x = x * x * x

p :: (Num a) => a -> a
p x = 3 * x - 4 * cube x

{-sine :: (Num a) => a -> a-}
sine :: Double -> Double
sine angle
    | abs angle < 0.1   = angle
    | otherwise         = p . sine $ angle / 3.0


main = do
    print $ sine (pi/6)
    print $ sine (pi/2)
    where pi = 3.14159265359
