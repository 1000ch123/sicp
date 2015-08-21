import Point
import Segment

-- |midpointSegment
-- >>> midpointSegment ((1,4),(2,8))
-- (1.5,6.0)
midpointSegment :: ((Float,Float),(Float,Float)) -> (Float,Float)
midpointSegment ((x1,y1),(x2,y2)) = makePoint ((x1 + x2) / 2) ((y1 + y2) /2)

main = do
    print $ midpointSegment ((1,2),(3,4))
