module Rectangle(
    makeRectangle,
    calcSquare,
    calcPeriometer
)where

import Point
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
calcSquare rect = dx * dy
    where x1 = xPoint . fst $ rect
          y1 = yPoint . fst $ rect
          x2 = xPoint . snd $ rect
          y2 = yPoint . snd $ rect
          dx = abs(x1-x2)
          dy = abs(y1-y2)

-- | calcSquare
-- >>> calcPeriometer ((1.0,5.0),(4.0,1.0))
-- 14.0
calcPeriometer :: ((Float,Float),(Float,Float)) -> Float
calcPeriometer rect = 2 * (dx + dy)
    where x1 = xPoint . fst $ rect
          y1 = yPoint . fst $ rect
          x2 = xPoint . snd $ rect
          y2 = yPoint . snd $ rect
          dx = abs(x1-x2)
          dy = abs(y1-y2)

