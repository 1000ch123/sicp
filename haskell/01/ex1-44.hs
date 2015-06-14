-- ex1-44
-- 関数のsmoothing
-- f(x)が与えられた時，
-- g(x) = f(x) + f(x+dx) + f(x-dx) / 3
-- をf(X)の値として用いる
import MyTypes

dx :: Double
dx = 0.0001

smooth :: Transform
smooth fn x = (fn x + fn (x + dx) + fn (x - dx)) / 3.0

smoothSquare :: Fn
smoothSquare = smooth (^2)

-- cf 1-43
-- N次畳み込み
repeated :: Int -> Transform -> Transform
repeated 1 tr = tr
repeated n tr = tr . repeated (n-1) tr

smoothN :: Int -> Transform
smoothN n = repeated n smooth

main = do
    print $ (^2) 300
    print $ smooth (^2) 300
    print $ smoothN 9 (^2) 300 -- n > 10で止まらなくなる..?
