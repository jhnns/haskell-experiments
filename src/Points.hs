module Points where

import Data.List (foldl')
import Point (Point)
import Sort (sort)
import Vector (angleBetween, diff)
import Debug.Trace (trace)

-- https://github.com/haskell/vector/blob/master/benchmarks/Algo/Quickhull.hs
-- https://en.wikipedia.org/wiki/Quickhull

convexHull :: [Point] -> [Point]
convexHull ps
    | length ps < 3 = []
    | length ps == 3 =
        let
            angle = angleBetween (head ps) (ps!!1) (ps!!2);
            isTriangle = angle /= 0 && angle /= 180;
            in if isTriangle then ps else []
    | otherwise =
        let
            l = length ps;
            c = center ps;
            shift ps@(p:_) = drop 1 (ps ++ [p]);
            rotate n xs = zipWith const (drop n (cycle xs)) xs;
            sorted0 = sort ps c;
            sortedR = rotate 1 sorted0;
            sortedL = rotate (l - 1) sorted0;
            -- triples = trace (show sortedL ++ "\n" ++ show sorted0 ++ "\n" ++ show sortedR) (zip3 sortedL sorted0 sortedR);
            triples = zip3 sortedL sorted0 sortedR;
            reducer ps (pl, p, pr) =
                let
                    angle = angleBetween p pl pr;
                    in if trace (show pl ++ show p ++ show pr ++ show angle) angle > 180 then p:ps else ps;
            in foldl' reducer [] triples

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
