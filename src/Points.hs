module Points where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Point (Point)
import Sort (sort)
import Vector (angle, diff)
import Debug.Trace (trace)

convexHull :: [Point] -> [Point]
convexHull ps
    | length ps < 3 = []
    | otherwise = let
        maxY p1@(_, y1) p2@(_, y2) = if y1 > y2 then p1 else p2;
        start = foldl1 maxY ps;
        circle1 = let
            sorted = sort ps (center ps);
            index = fromJust $ elemIndex start sorted;
            in drop (index + 1) (cycle sorted);
        circle2 = drop 1 circle1;
        walk _ 0 hull _ _ = hull;
        walk circle i hull p1 p2 = let
            p3 = head circle;
            phi = angle (diff p1 p2) (diff p3 p2);
            next = walk (tail circle) (i - 1);
            save = next (p2:hull) p2 p3;
            skip = next hull p1 p3;
            in if phi > 180 then save else skip;
        in walk circle2 (length ps) [] start (head circle1)

center :: [Point] -> Point
center [] = error "cannot find center of an empty list"
center ps@(p1:_)
    | length ps == 1 = p1
    | length ps == 2 = let
        (x, y) = p1;
        (x', y') = diff p1 (last ps);
        in (x + x' / 2, y + y' / 2)
    | otherwise = let
        xs = map fst ps;
        ys = map snd ps;
        in center [(minimum xs, minimum ys), (maximum xs, maximum ys)]
