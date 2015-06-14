-- p76
-- newton method
-- Floatだと精度が足りない

import FixedPoint

dx :: (Num a, Fractional a) => a
dx = 0.00001

deriv :: (Num a, Fractional a) => (a -> a) -> a -> a
deriv fn x = (fn (x + dx) - fn x) / dx

newtonTrasnform :: (Double -> Double) -> Double -> Double
newtonTrasnform fn x = x - fn x / fn' x
    where fn' = deriv fn

-- f(x)=0を解く
-- f(x)=0の解は，newtonTransformされた関数fnNewtonに対し不動点となる
-- (ここ数学.僕が知ってるのと違う)
-- これはfnを変形させた x=g(x)の不動点計算よりも早く収束するらしいよ
newton :: (Double -> Double) -> Double -> Double
newton fn = fixedPoint fnNewton
    where fnNewton = newtonTrasnform fn

newtonSqrt :: Double -> Double
newtonSqrt a = newton (\x -> x^2 - a) 1.0

main = do
    print $ deriv (^3) 5
    print $ newtonSqrt 5.0
