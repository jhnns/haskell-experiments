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
        l = length ps;
        c = center ps;
        start = top ps;
        circle = let
            sorted = sort ps c;
            index = fromJust $ elemIndex start sorted;
            in drop index (cycle sorted);
        walk 0 _ ps _ _ = ps;
        walk i circle hull p1 p2  = let
            p3 = head circle;
            phi = angle (diff p1 p2) (diff p3 p2);
            spin = walk (i - 1) (tail circle);
            save = spin (p2:hull) p2 p3;
            skip = spin hull p1 p3;
            in if phi > 180 then save else skip;
        in walk l (drop 2 circle) [] start (circle !! 1)

center :: [Point] -> Point
center ps
    -- no need to check for length = 0, foldl1 will throw on an empty list anyway
    | length ps == 1 = head ps
    | length ps == 2 =
        let
            p1@(x, y) = head ps;
            (x', y') = diff p1 (last ps);
            in (x + x' / 2, y + y' / 2)
    | otherwise =
        let
            minX = fst $ left ps;
            maxX = fst $ right ps;
            minY = snd $ bottom ps;
            maxY = snd $ top ps;
            in center [(minX, minY), (maxX, maxY)]

top :: [Point] -> Point
top = foldl1 maxY

maxY :: Point -> Point -> Point
maxY p1@(_, y1) p2@(_, y2) = if y1 > y2 then p1 else p2

right :: [Point] -> Point
right = foldl1 maxX

maxX :: Point -> Point -> Point
maxX p1@(x1, _) p2@(x2, _) = if x1 > x2 then p1 else p2

bottom :: [Point] -> Point
bottom = foldl1 minY

minY :: Point -> Point -> Point
minY p1@(_, y1) p2@(_, y2) = if y1 < y2 then p1 else p2

left :: [Point] -> Point
left = foldl1 minX

minX :: Point -> Point -> Point
minX p1@(x1, _) p2@(x2, _) = if x1 < x2 then p1 else p2
