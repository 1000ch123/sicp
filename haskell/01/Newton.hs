module Newton(
    newton,
)where

import MyTypes
import FixedPoint

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

-- how to use
-- solve x^2 = a
-- fn = x^2 - a = 0
sampleSqrt :: Fn
sampleSqrt a = newton (\x -> x^2 - a) 1.0
