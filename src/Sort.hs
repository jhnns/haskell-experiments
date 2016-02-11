module Sort where

import Data.List (sortBy)
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
compareRadian p1 p2 = compare (radian p1) (radian p2)

radian :: Point -> Float
radian (x, y)
    | x == 0 && y == 0 = 100 -- just an arbitrary value that won't clash with atan2 returned values
    | otherwise = atan2 y x