-- ex1-36
-- x^x = 1000 の解を求めよ

import FixedPoint

main = do
    print $ fixedPoint (\x -> (log 1000) / (log x)) 2.0
