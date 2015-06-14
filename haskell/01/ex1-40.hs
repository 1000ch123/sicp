import MyTypes
import Newton

cubic :: Double -> Double -> Double -> Fn
cubic a b c x = x^3 + a * x^2 + b * x + c

newtonCubic :: Double -> Double -> Double -> Fn
newtonCubic a b c = newton (cubic a b c)

main = do
    print $ newtonCubic 1 1 1 1.0
