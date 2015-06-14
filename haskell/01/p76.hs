import FixedPoint

type Fn = (Double -> Double)
type Transform = Fn -> Fn

-- p75
-- 半区間法でのいろいろ
averageDamp :: Transform
averageDamp fn x = (fn x + x) / 2

halfMethodSqrt :: Fn
halfMethodSqrt x = fixedPoint (averageDamp (\y -> x / y)) 1.0

-- p76
-- newton method
-- Floatだと精度が足りない

-- 精度.const
dx :: Double
dx = 0.00001

-- びぶん
deriv :: Transform
deriv fn x = (fn (x + dx) - fn x) / dx


-- f(x)=0を解く
-- f(x)=0の解は，newtonTransformされた関数fnNewtonに対し不動点となる
-- (ここ数学.僕が知ってるのと違う)
-- これはfnを変形させた x=g(x)の不動点計算よりも早く収束するらしいよ
newtonTransform :: Transform
newtonTransform fn x = x - fn x / fn' x
    where fn' = deriv fn

newton :: Transform
newton fn = fixedPoint fnNewton
    where fnNewton = newtonTransform fn

-- つかってみる
newtonMethodSqrt :: Fn
newtonMethodSqrt a = newton (\x -> x^2 - a) 1.0

-- p77
-- 平均法，newton法でのfixedPointを生成する
-- ターゲット関数，不動点求める関数へのコンバート関数，初期値を与える
fixedPointOfTransform :: Fn -> Transform -> Fn
fixedPointOfTransform fn trans = fixedPoint fnTrans
    where fnTrans = trans fn

halfMethodSqrt2 :: Fn
halfMethodSqrt2 a = fixedPointOfTransform (a/) averageDamp 1.0

newtonMethodSqrt2 :: Fn
newtonMethodSqrt2 a = fixedPointOfTransform (\x -> x^2 - a) newtonTransform 1.0

main = do
    print $ halfMethodSqrt 5.0
    print $ halfMethodSqrt2 5.0
    print $ newtonMethodSqrt 5.0
    print $ newtonMethodSqrt2 5.0
