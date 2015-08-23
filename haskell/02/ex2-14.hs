import Interval

type Interval = (Float, Float)

par1 :: Interval -> Interval -> Interval
par1 i1 i2 = divInterval (mulInterval i1 i2) (addInterval i1 i2)

par2 :: Interval -> Interval -> Interval
par2 i1 i2 = divInterval one (addInterval (divInterval one i1) (divInterval one i2))
    where one = makeInterval 1 1

interval1 = makeCenterPercent 100 1
interval2 = makeCenterPercent 200 2


main = do
    print $ percent $ par1 interval1 interval2
    print $ percent $ par2 interval1 interval2
    print $ percent $ par1 interval1 interval1
    print $ percent $ par2 interval1 interval1
