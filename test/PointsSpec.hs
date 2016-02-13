module Main where

import Points (convexHull, center)
import Point (Point)
import Support (shouldEql)

p00 = (0, 0)
p10 = (1, 0)
p10' = (-1, 0)
p01 = (0, 1)
p01' = (0, -1)
p11 = (1, 1)
p11' = (-1, -1)
p20 = (2, 0)
p02 = (0, 2)

main :: IO ()
main = print $ foldl1 (++) [

    center [p00] `shouldEql` p00,
    center [p10', p10] `shouldEql` p00,
    center [p01', p01] `shouldEql` p00,
    center [p01', p01, p10] `shouldEql` (0.5, 0),
    center [
        (-1, 1),    p11,
        p11',       (1, -1)
    ] `shouldEql` p00,
    center [
        (-1, 1),    p11,
                                p20,
        p11',       (1, -1)
    ] `shouldEql` (0.5, 0),

    convexHull [] `shouldEql` [],
    convexHull [p00] `shouldEql` [],
    convexHull [p00, p01] `shouldEql` [],

    convexHull [p00, p01, p01'] `shouldEql` [],
    convexHull [p01, p01', p00] `shouldEql` [],
    convexHull [p01', p00, p01] `shouldEql` [],
    convexHull [p00, p10, p10', p20] `shouldEql` [],
    convexHull [p00, p01, p01', p02] `shouldEql` [],

    convexHull [p00, p01, p10] `shouldEql` [p01, p10, p00],
    convexHull [p01, p10, p00] `shouldEql` [p01, p10, p00],
    convexHull [p10, p00, p01] `shouldEql` [p01, p10, p00],

    convexHull [
        (-1,  1),   ( 1,  1),
        (-1, -1),   ( 1, -1)
    ] `shouldEql` [(1, 1), (1, -1), (-1, -1), (-1, 1)],

    convexHull [
                    (-1,  1),               ( 1,  1),
                                                        ( 2,  0),
                    (-1, -1),               ( 1, -1)
    ] `shouldEql` [(1, 1), (2, 0), (1, -1), (-1, -1), (-1, 1)],

    convexHull [
                    (-1,  1),   ( 0,  1),   ( 1, 1),
                    (-0.5, 0),  ( 0,  0),   (1.5, 0),
                    (-1, -1),   ( 0, -1)
    ] `shouldEql` [(1, 1), (1.5, 0), (0, -1), (-1, -1), (-1, 1)],

    convexHull [
        (-2,  2),                                       ( 2,  2),
                    (-1,  1),   ( 0,  1),   ( 1,  1),
                    (-1,  0),   ( 0,  0),   ( 1,  0),
                    (-1, -1),   ( 0, -1),   ( 1, -1),
        (-2, -2),                                       ( 2, -2)
    ] `shouldEql` [(2, 2), (2, -2), (-2, -2), (-2, 2)],

    convexHull [
        (-2,  2),
                    (-1,  1),
                                ( 0,  0),
                                            ( 1, -1),
                                                        ( 2, -2)
    ] `shouldEql` [],

    convexHull [
                                ( 0,  2),
                    (-1,  1),               ( 1,  1),

                    (-1, -1),               ( 1, -1),
                                ( 0, -2)
    ] `shouldEql` [(0, 2), (1, 1), (1, -1), (0, -2), (-1, -1), (-1, 1)],

    convexHull [
        (-2,  0),               ( 0, -0.5),             ( 2,  0),
                    (-1, -1),               ( 1, -1),
                                ( 0, -2)
    ] `shouldEql` [(2, 0), (0, -2), (-2, 0)],

    convexHull [
        (-2,  0),               ( 0, -0.5),             ( 2,  0),
                    (-1, -1.5),             ( 1, -1.5),
                                ( 0, -2)
    ] `shouldEql` [(2, 0), (1, -1.5), (0, -2), (-1, -1.5), (-2, 0)],

    convexHull [
                                ( 0,  1),
        (-2,  0),               ( 0, -0.5),             ( 2,  0),
                    (-1, -1),               ( 1, -1),
                                ( 0, -2),
                                ( 0, -3)
    ] `shouldEql` [(0, 1), (2, 0), (0, -3), (-2, 0)],

    "done"

    ]
