module Vector where

import Debug.Trace (trace)

type Vector = (Float, Float)

rad :: Float
rad = pi / 180

angleBetween :: Vector -> Vector -> Float
angleBetween v1 v2
    | m1 == 0 || m2 == 0 || v1 == v2 = 0
    | otherwise = let
        angle = deg . acos $ (v1 `dot` v2) / (m1 * m2);
        in if orient v1 v2 == -1 then 360 - angle else angle
    where
        m1 = magn v1
        m2 = magn v2

diff :: Vector -> Vector -> Vector
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

dot :: Vector -> Vector -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

magn :: Vector -> Float
magn (x, y) = sqrt $ x^2 + y^2

deg :: Float -> Float
deg a = a / rad

orient :: Vector -> Vector -> Float
orient (x1, y1) (x2, y2) = signum $ x1 * y2 - y1 * x2
