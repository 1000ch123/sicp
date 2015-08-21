module Segment(
    makeSegment,
    startSegment,
    endSegment
)where

-- |makeSegment
-- >>> makeSegment (1,2) (3,4)
-- ((1.0,2.0),(3.0,4.0))
makeSegment :: (Float, Float) -> (Float, Float) -> ((Float,Float),(Float,Float))
makeSegment a b = (a,b)

-- |startSegment
-- >>> startSegment ((1,2),(3,4))
-- (1.0,2.0)
startSegment :: ((Float,Float),(Float,Float)) -> (Float, Float)
startSegment = fst

-- |endSegment
-- >>> endSegment ((1,2),(3,4))
-- (3.0,4.0)
endSegment :: ((Float,Float),(Float,Float)) -> (Float, Float)
endSegment = snd
