-----------------------------------------------------------------------------
-- |
-- Module      :  Sort
-- License     :  Unlicense
--
-- Maintainer  :  mail@johannesewald.de
--
-- Provides a method to sort points in a 2-dimensional
-- space around a given center.
--
-----------------------------------------------------------------------------
module Sort (
    sort
    ) where

import Data.List (sortBy)
import Vector (Vector, magn, diff)
import Point (Point)
import Debug.Trace (trace)

{-|
    Sorts the given points around a center by comparing their direction.
    Returns a list of points sorted counter-clockwise, starting from the x-axi
    in the lower left quadrant. If two points have the same direction, their
    magnitude is compared with smaller magnitudes taking precedence. The latter
    behavior is especially important in order to detect non-convex points easily
    when encountering two points with the same direction.
-}
sort :: [Point] -> Point -> [Point]
sort ps c@(cx, cy)
    | length ps < 2 = ps
    | otherwise = let
        move = diff c;
        dirAndMagn p1 p2 = comparePoints (move p1) (move p2);
        in sortBy dirAndMagn ps

{-|
    Used as comparison function for sortBy.
-}
comparePoints :: Point -> Point -> Ordering
comparePoints p1 p2 = let
    phi (x, y) = atan2 y x
    direction = compare (phi p1) (phi p2);
    magnitude = compare (magn p1) (magn p2);
    in if direction == EQ then magnitude else direction
