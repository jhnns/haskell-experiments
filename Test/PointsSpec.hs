module Main where

import Points (convexHull)
import Point (Point)
import Support (shouldEql)

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

    convexHull [] `shouldEql` []

    "done"

    ]
