module Rectangle(
    makeRectangle,
    calcSquare,
    calcPeriometer
)where

-- 独自データ型で書きたいけど，プリミティブだけで頑張るぞい


-- |makeRectangle
-- >>> makeRectangle (1,2) (3,4)
-- ((1.0,2.0),(3.0,4.0))
makeRectangle :: (Float,Float) -> (Float,Float) -> ((Float,Float),(Float,Float))
makeRectangle a b = (a,b)

-- | calcSquare
-- >>> calcSquare ((1.0,5.0),(4.0,1.0))
-- 12.0
calcSquare :: ((Float,Float),(Float,Float)) -> Float
calcSquare ((x1,y1),(x2,y2)) = dx * dy
    where dx = abs(x1-x2)
          dy = abs(y1-y2)

-- | calcSquare
-- >>> calcPeriometer ((1.0,5.0),(4.0,1.0))
-- 14.0
calcPeriometer :: ((Float,Float),(Float,Float)) -> Float
calcPeriometer ((x1,y1),(x2,y2)) = 2 * (dx + dy)
    where dx = abs(x1-x2)
          dy = abs(y1-y2)

