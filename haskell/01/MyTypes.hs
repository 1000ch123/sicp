module MyTypes(
    Fn,
    Transform
)where

type Fn = (Double -> Double)
type Transform = Fn -> Fn

