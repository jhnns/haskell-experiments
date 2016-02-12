module Vector where

import Point (Point)
import Debug.Trace (trace)

p00 :: Point
p00 = (0, 0)

rad :: Float
rad = pi / 180

angleBetween :: Point -> Point -> Point -> Float
angleBetween c@(cx, cy) p1@(p1x, p1y) p2@(p2x, p2y)
    | c == p1 || c == p2 || p1 == p2 = 0
    | otherwise = if orient c p1 p2 == -1 then 360 - angle else angle
    where
        angle = deg . acos $ (v1 `dot` v2) / (magn v1 * magn v2)
        v1 = diff p1 c
        v2 = diff p2 c

diff :: Point -> Point -> Point
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

magn :: Point -> Float
magn (x, y) = sqrt $ x^2 + y^2

deg :: Float -> Float
deg a = a / rad

orient :: Point -> Point -> Point -> Float
orient c p1 p2 = signum $ x1 * y2 - y1 * x2
    where
        x1 = fst v1
        y1 = snd v1
        x2 = fst v2
        y2 = snd v2
        v1 = diff p1 c
        v2 = diff p2 c
