module Point(
    printPoint,
    makePoint,
    xPoint,
    yPoint
)where

-- |printPoint
-- >>> printPoint (1,2)
-- "-- print point --"
-- "x"
-- 1.0
-- "y"
-- 2.0
printPoint :: (Float, Float) -> IO()
printPoint (a,b) = do
    print "-- print point --"
    print "x"
    print a
    print "y"
    print b

-- |makePoint
-- >>> makePoint 1 2
-- (1.0,2.0)
makePoint :: Float -> Float -> (Float, Float)
makePoint a b = (a,b)

-- |xPoint
-- >>> xPoint (1,2)
-- 1.0
xPoint :: (Float, Float) -> Float
xPoint = fst

-- |yPoint
-- >>> yPoint (1,2)
-- 2.0
yPoint :: (Float, Float) -> Float
yPoint = snd

