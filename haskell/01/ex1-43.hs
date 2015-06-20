-- ex1-43
-- 指定関数をn回繰り返す関数

type Fn = Int -> Int
type Transform = Fn -> Fn

repeated :: Int -> Transform
repeated 0 _  = id
repeated n fn = fn . repeated (n-1) fn

doubleSquare :: Fn
doubleSquare = repeated 2 (^2)

main = do
    print $ doubleSquare 5
