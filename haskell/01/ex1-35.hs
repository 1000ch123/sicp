-- ex1-35
-- 黄金比率

-- φ^2 = φ+1
-- は
-- x |-> 1 + 1/x
-- の不動点であることを示せ

-- 両辺*x
-- x^2 -> x + 1

import FixedPoint

main = do
    print $ fixedPoint (\x -> 1 + (1/x)) 1.0

