module Interval(
    makeInterval,
    lowerBound,
    upperBound,
    addInterval,
    subInterval,
    mulInterval,
    divInterval
)where

type Interval = (Float, Float)

-- | makeInterval
-- >>> makeInterval 8 12
-- (8.0,12.0)
makeInterval :: Float -> Float -> Interval
makeInterval a b = (a,b)

-- | lowerBound
-- >>> lowerBound $ makeInterval 8 12
-- 8.0
lowerBound :: Interval -> Float
lowerBound (a,b) = min a b

-- | upperBound
-- >>> upperBound $ makeInterval 8 12
-- 12.0
upperBound :: Interval -> Float
upperBound (a,b) = max a b

-- 四則演算
-- | addInterval
-- >>> addInterval (8,12) (12,18)
-- (20.0,30.0)
addInterval :: Interval -> Interval -> Interval
addInterval i1 i2 = makeInterval (lowerBound i1 + lowerBound i2) (upperBound i1 + upperBound i2)


-- | subInterval
-- >>> subInterval (8,12) (12,18)
-- (-10.0,0.0)
subInterval :: Interval -> Interval -> Interval
subInterval i1 i2 = makeInterval (minimum candidates) (maximum candidates)
    where p1 = lowerBound i1 - lowerBound i2
          p2 = lowerBound i1 - upperBound i2
          p3 = upperBound i1 - lowerBound i2
          p4 = upperBound i1 - upperBound i2
          candidates = [p1,p2,p3,p4]

-- | mulInterval
-- >>> mulInterval (8,12) (12,18)
-- (96.0,216.0)
mulInterval :: Interval -> Interval -> Interval
mulInterval i1 i2 = makeInterval (minimum candidates) (maximum candidates)
    where p1 = lowerBound i1 * lowerBound i2
          p2 = lowerBound i1 * upperBound i2
          p3 = upperBound i1 * lowerBound i2
          p4 = upperBound i1 * upperBound i2
          candidates = [p1,p2,p3,p4]

-- | divInterval
-- >>> divInterval (8,12) (2,4)
-- (2.0,6.0)
divInterval :: Interval -> Interval -> Interval
divInterval i1 i2 = mulInterval i1 i2'
    where i2' = makeInterval (1 / upperBound i2) (1 / lowerBound i2)

