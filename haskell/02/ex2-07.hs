import Interval

main = do
    print $ makeInterval 8 12
    print $ lowerBound $ makeInterval 8 12
    print $ upperBound $ makeInterval 8 12
