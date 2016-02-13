-----------------------------------------------------------------------------
-- |
-- Module      :  Points
-- License     :  Unlicense
--
-- Maintainer  :  mail@johannesewald.de
--
-- Provides methods to work with points in a 2-dimensional space.
-- All coordinates are in single float precision.
--
-----------------------------------------------------------------------------

module Points (
    convexHull,
    center
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Point (Point)
import Sort (sort)
import Vector (angle, diff)
import Debug.Trace (trace)

{-|
    Returns a subset of the given list which contains all points that
    form the convex hull. That is, no outer angle is smaller than 180°.
    The returned list is sorted clockwise, starting with the point with the
    greatest y coordinate. Returns an empty list if an empty list is given or
    all points are located on a line.
-}
convexHull :: [Point] -> [Point]
convexHull ps
    | length ps < 3 = []
    | otherwise = let
        -- 1. Find the starting point p1 that is guaranteed on the convex hull.
        -- (There is one edge case: If we only have three points and all points
        -- are located on a line, there is no convex hull and thus `start` is
        -- NOT on the convex hull.)
        maxY = let
            maxY p1@(_, y1) p2@(_, y2) = if y1 > y2 then p1 else p2;
            in foldl1 maxY ps;
        circle = let
            -- 2. Sort all points around the center of the point cloud.
            sorted = sort ps (center ps);
            -- 3. In order to walk all points, we use a cycled list which is
            -- rotated until the first element after the starting point is
            -- the head.
            index = fromJust $ elemIndex maxY sorted;
            in drop (index + 1) (cycle sorted)
        -- 4. Now walk around the circle and save all points to our `hull` list
        -- that have an outer angle greater than 180. We use an iteration
        -- counter i in order to track whether we have walked around the whole
        -- circle. In that case we just return `hull` as result.
        walk _ 0 hull _ _ = hull;
        walk circle i hull pl p = let
            -- Measure the outer angle at p between pl(eft) and pr(ight).
            pr = head circle;
            phi = angle (diff pl p) (diff pr p);
            -- Prepare the next step
            next = walk (tail circle) (i - 1);
            -- Saves p into hull and proceeds with p as pl and pr as new p
            save = next (p:hull) p pr;
            -- Discards p, keeps pl and proceeds with pr as new p
            skip = next hull pl pr;
            in if phi > 180 then save else skip;
        in walk
            (drop 1 circle)     -- circle
            (length ps)         -- iteration counter
            []                  -- hull
            maxY                -- pl
            (head circle)       -- p

{-|
    Returns the center of the given point cloud. The center is defined by the
    intersection of the both diagonals of the smallest rectangle that enfolds
    all points.
-}
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
