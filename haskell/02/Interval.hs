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
makeInterval a b = (min a b, max a b)

-- | lowerBound
-- >>> lowerBound $ makeInterval 8 12
-- 8.0
lowerBound :: Interval -> Float
lowerBound = fst

-- | upperBound
-- >>> upperBound $ makeInterval 8 12
-- 12.0
upperBound :: Interval -> Float
upperBound = snd

width :: Interval -> Float
width (a,b) = (a + b) / 2

-- 四則演算
calcInterval :: (Float -> Float -> Float) -> Interval -> Interval -> Interval
calcInterval op i1 i2 =  makeInterval (minimum candidates) (maximum candidates)
    where p1 = op (lowerBound i1) (lowerBound i2)
          p2 = op (lowerBound i1) (upperBound i2)
          p3 = op (upperBound i1) (lowerBound i2)
          p4 = op (upperBound i1) (upperBound i2)
          candidates = [p1,p2,p3,p4]

-- | addInterval
-- >>> addInterval (8,12) (12,18)
-- (20.0,30.0)
addInterval :: Interval -> Interval -> Interval
addInterval = calcInterval (+)

-- | subInterval
-- >>> subInterval (8,12) (12,18)
-- (-10.0,0.0)
subInterval :: Interval -> Interval -> Interval
subInterval = calcInterval (-)

-- | mulInterval
-- >>> mulInterval (8,12) (12,18)
-- (96.0,216.0)
mulInterval :: Interval -> Interval -> Interval
mulInterval = calcInterval (*)

-- | divInterval
-- >>> divInterval (8,12) (2,4)
-- (2.0,6.0)
-- >>> divInterval (8,12) (0,4)
-- *** Exception: Division by zero
divInterval :: Interval -> Interval -> Interval
divInterval _ (0, _) = error "Division by zero"
divInterval _ (_, 0) = error "Division by zero"
divInterval a b = calcInterval (/) a b

-- 頑張って実装するversion
{-addInterval i1 i2 = makeInterval (lowerBound i1 + lowerBound i2) (upperBound i1 + upperBound i2)-}

{-subInterval i1 i2 = makeInterval (minimum candidates) (maximum candidates)-}
    {-where p1 = lowerBound i1 - lowerBound i2-}
          {-p2 = lowerBound i1 - upperBound i2-}
          {-p3 = upperBound i1 - lowerBound i2-}
          {-p4 = upperBound i1 - upperBound i2-}
          {-candidates = [p1,p2,p3,p4]-}

{-mulInterval i1 i2 = makeInterval (minimum candidates) (maximum candidates)-}
    {-where p1 = lowerBound i1 * lowerBound i2-}
          {-p2 = lowerBound i1 * upperBound i2-}
          {-p3 = upperBound i1 * lowerBound i2-}
          {-p4 = upperBound i1 * upperBound i2-}
          {-candidates = [p1,p2,p3,p4]-}

{-divInterval i1 i2 = mulInterval i1 i2'-}
    {-where i2' = makeInterval (1 / upperBound i2) (1 / lowerBound i2)-}

