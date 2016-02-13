module Main where

import Sort (sort)
import Point (Point)
import Support (shouldEql)

p00 = (0, 0)
p10 = (1, 0)
p10' = (-1, 0)
p01 = (0, 1)
p01' = (0, -1)
p11 = (1, 1)
p11' = (-1, -1)
p13 = (1, 3)

main :: IO ()
main = print $ foldl1 (++) [

    sort [] p00 `shouldEql` [],
    sort [p00] p00 `shouldEql` [p00],
    sort [p00, p00] p00 `shouldEql` [p00, p00],
    sort [p00, p10] p00 `shouldEql` [p00, p10],
    sort [p10, p10] p00 `shouldEql` [p10, p10],
    sort [p10, p00] p00 `shouldEql` [p00, p10],
    sort [p10', p10, p00] p00 `shouldEql` [p00, p10, p10'],
    sort [
        p10', p10, p11, p01, p01', p01, p00, p10', p11'
    ] p00 `shouldEql` [
        p11', p01', p00, p10, p11, p01, p01, p10', p10'
    ],

    sort [] p13 `shouldEql` [],
    sort [p00] p13 `shouldEql` [p00],
    sort [p00, p00] p13 `shouldEql` [p00, p00],
    sort [p00, p10] p13 `shouldEql` [p00, p10],
    sort [
        p10', p10, p11, p01, p01', p01, p00, p10', p11'
    ] p13 `shouldEql` [
        p10', p10', p01, p01, p11', p00, p01', p11, p10
    ],

    sort [p10, p00, p10'] (-2, 0) `shouldEql` [p10', p00, p10],

    "done"

    ]
