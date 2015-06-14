module HalfInterval(
    halfInterval
) where

import MyTypes
import FixedPoint

averageDamp :: Transform
averageDamp fn x = (fn x + x) / 2

halfInterval :: Transform
halfInterval fn = fixedPoint (averageDamp fn)

-- how to use
-- solve x^2 = a
-- calc fixed point : x |-> a / x
sampleSqrt :: Fn
sampleSqrt a = halfInterval (\x -> a / x) 1.0

