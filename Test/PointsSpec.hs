module Main where

import Points (convexHull, center, top, right, bottom, left)
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

ps1 = [
    (-2, 2),   (-1, 2),   (0, 2),   (1, 2),   (2, 2),
    (-2, 1),   (-1, 1),   (0, 1),   (1, 1),   (2, 1),
    (-2, 0),   (-1, 0),   (0, 0),   (1, 0),   (2, 0),
    (-2, -1),  (-1, -1),  (0, -1),  (1, -1),  (2, -1)
    ]

ps1Expected = [
    (-2, 2),   (-1, 2),   (0, 2),   (1, 2),   (2, 2),
    (-2, 1),                                  (2, 1),
    (-2, 0),                                  (2, 0),
    (-2, -1),  (-1, -1),  (0, -1),  (1, -1),  (2, -1)
    ]

main :: IO ()
main = print $ foldl1 (++) [

    top [p00] `shouldEql` p00,
    top [p00, p01] `shouldEql` p01,
    top [p00, p10] `shouldEql` p10,
    top [p00, p01'] `shouldEql` p00,
    top [p00, p00] `shouldEql` p00,
    top [p11', p00, p11] `shouldEql` p11,

    right [p00] `shouldEql` p00,
    right [p00, p01] `shouldEql` p01,
    right [p00, p10] `shouldEql` p10,
    right [p00, p10'] `shouldEql` p00,
    right [p00, p00] `shouldEql` p00,
    right [p10', p00, p10] `shouldEql` p10,

    bottom [p00] `shouldEql` p00,
    bottom [p00, p01] `shouldEql` p00,
    bottom [p00, p10] `shouldEql` p10,
    bottom [p00, p01'] `shouldEql` p01',
    bottom [p00, p00] `shouldEql` p00,
    bottom [p11', p00, p11] `shouldEql` p11',

    left [p00] `shouldEql` p00,
    left [p00, p01] `shouldEql` p01,
    left [p00, p10] `shouldEql` p00,
    left [p00, p10'] `shouldEql` p10',
    left [p00, p00] `shouldEql` p00,
    left [p10', p00, p10] `shouldEql` p10',

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

    -- convexHull [] `shouldEql` [],
    -- convexHull [p00] `shouldEql` [],
    -- convexHull [p00, p01] `shouldEql` [],
    --
    -- convexHull [p00, p01, p01'] `shouldEql` [],
    -- convexHull [p01, p01', p00] `shouldEql` [],
    -- convexHull [p01', p00, p01] `shouldEql` [],
    -- convexHull [p00, p10, p10', p20] `shouldEql` [],
    -- convexHull [p00, p01, p01', p02] `shouldEql` [],
    --
    -- convexHull [p00, p01, p10] `shouldEql` [p00, p01, p10],
    -- convexHull [p01, p10, p00] `shouldEql` [p01, p10, p00],
    -- convexHull [p10, p00, p01] `shouldEql` [p10, p00, p01],

    -- convexHull [
    --     (-1, 1),    p11,
    --     p11',       (1, -1)
    -- ] `shouldEql` [(-1, 1), p11, (1, -1), p11'],
    --
    -- convexHull [
    --     (-1, 1),    p11,
    --                             p20,
    --     p11',       (1, -1)
    -- ] `shouldEql` [(-1, 1), p11, p20, (1, -1), p11'],

    convexHull [
        (-1, 1),    p01,        p11,
        (-0.5, 0),  p00,        (1.5, 0),
        p11',       p01'
    ] `shouldEql` [(-1, 1), p01, p11, (1.5, 0), p01', p11'],


    -- convexHull [p01', p00, p10] `shouldEql` [p01', p00, p10]

    "done"

    ]
