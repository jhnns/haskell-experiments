module Sort where

import Data.List (sortBy)
import Vector (magn)
import Point (Point)
import Debug.Trace (trace)

sort :: [Point] -> Point -> [Point]
sort [] _ = []
sort ps c@(cx, cy)
    | length ps == 1 = ps
    | otherwise = ps''
    where
        ps' = map move ps
        move = translate (-cx, -cy)
        sorted = sortBy compareRadian ps'
        ps'' = map moveBack sorted
        moveBack = translate c

translate :: Point -> Point -> Point
translate (vx, vy) (px, py) = (px + vx, py + vy)

compareRadian :: Point -> Point -> Ordering
compareRadian p1 p2 = let
    r = compare (radian p1) (radian p2);
    m = compare (magn p1) (magn p2);
    in if r == EQ then m else r

radian :: Point -> Float
radian (x, y) = atan2 y x
