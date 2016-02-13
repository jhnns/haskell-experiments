module Main where

import Vector (angle, orient)
import Support (shouldEql)

p00 = (0, 0)
p10 = (1, 0)
p10' = (-1, 0)
p01 = (0, 1)
p01' = (0, -1)
p11 = (1, 1)
p11' = (-1, -1)
cw = -1
ccw = 1

main :: IO ()
main = print $ foldl1 (++) [
    orient p10 p01 `shouldEql` ccw,
    orient p01 p10 `shouldEql` cw,
    orient p10 p10' `shouldEql` 0,
    orient (-1, 1) p11' `shouldEql` ccw,
    orient p10 p00 `shouldEql` 0,

    angle p00 p00 `shouldEql` 0,
    angle p01 p00 `shouldEql` 0,
    angle p00 p01 `shouldEql` 0,

    angle p10 p01 `shouldEql` 90,
    angle p01 p10 `shouldEql` 270,

    angle p01 p01' `shouldEql` 180,
    angle p01' p01 `shouldEql` 180,

    angle p10' p10 `shouldEql` 180,
    angle p10 p10' `shouldEql` 180,

    "done"

    ]
