-----------------------------------------------------------------------------
-- |
-- Module      :  Vector
-- License     :  Unlicense
--
-- Maintainer  :  mail@johannesewald.de
--
-- Provides methods to work with 2-dimensional vectors.
--
-----------------------------------------------------------------------------
module Vector (
    Vector,
    angle,
    magn,
    diff,
    dot,
    deg,
    orient
    ) where

import Point (Point)
import Debug.Trace (trace)

{-|
    A tuple of x and y which describe a vector in a 2-dimensional space.
-}
type Vector = (Float, Float)

{-|
    A constant used to translate radians into degrees and vice-versa.
-}
rad :: Float
rad = pi / 180

{-|
    Calculates the angle between two vectors.
-}
angle :: Vector -> Vector -> Float
angle v1 v2
    | m1 == 0 || m2 == 0 || v1 == v2 = 0
    | otherwise = let
        angle = deg . acos $ (v1 `dot` v2) / (m1 * m2);
        in if orient v1 v2 == -1 then 360 - angle else angle
    where
        m1 = magn v1
        m2 = magn v2

{-|
    Caculates the vector from one pointer to another.
-}
diff :: Point -> Point -> Vector
diff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

{-|
    Returns the result of the dot multiplication of two vectors.
-}
dot :: Vector -> Vector -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

{-|
    Returns the length of a vector.
-}
magn :: Vector -> Float
magn (x, y) = sqrt $ x^2 + y^2

{-|
    Translates radians into degrees.
-}
deg :: Float -> Float
deg r = r / rad

{-|
    Returns the orientation of the vector that goes from one point to another.
    The result is one of -1.0, 0.0 and 1.0, meaning clockwise,
    equal and counter-clockwise respectively.
-}
orient :: Point -> Point -> Float
orient (x1, y1) (x2, y2) = signum $ x1 * y2 - y1 * x2
