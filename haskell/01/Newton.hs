module Newton(
    newton,
    Fn,
    Transform
)where

import FixedPoint

type Fn = (Double -> Double)
type Transform = Fn -> Fn

dx :: Double
dx = 0.00001

deriv :: Transform
deriv fn x = (fn (x + dx) - fn x) / dx

newtonTransform :: Transform
newtonTransform fn x = x - fn x / fn' x
    where fn' = deriv fn

newton :: Transform
newton fn = fixedPoint fnNewton
    where fnNewton = newtonTransform fn


sampleSqrt :: Fn
sampleSqrt a = newton (\x -> x^2 - a) 1.0
