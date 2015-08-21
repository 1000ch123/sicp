import Rectangle

main = do
    print $ calcSquare rect
    print $ calcPeriometer rect
    where rect = makeRectangle (1,4) (3,6)
