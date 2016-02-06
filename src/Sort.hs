module Sort where

import Data.List (sortBy)
import Data.Point (Point)

compareAngle :: (Num a) => Point -> Point -> Point -> a
compareAngle a b c = 0

sortClockwise :: [Point] -> Point -> [Point]
sortClockwise [] _ = []
sortClockwise ps c
    | length ps == 1 = ps
