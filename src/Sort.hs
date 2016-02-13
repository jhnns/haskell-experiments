module Sort where

import Data.List (sortBy)
import Vector (Vector, magn, diff)
import Point (Point)
import Debug.Trace (trace)

sort :: [Point] -> Point -> [Point]
sort [] _ = []
sort ps c@(cx, cy)
    | length ps == 1 = ps
    | otherwise = let
        move = diff c;
        radian p1 p2 = compareRadian (move p1) (move p2);
        in sortBy radian ps

compareRadian :: Point -> Point -> Ordering
compareRadian p1 p2 = let
    r = compare (radian p1) (radian p2);
    m = compare (magn p1) (magn p2);
    in if r == EQ then m else r

radian :: Point -> Float
radian (x, y) = atan2 y x
