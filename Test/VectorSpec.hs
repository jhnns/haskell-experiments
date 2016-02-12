module Main where

import Vector (angleBetween, orient)
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
    orient p00 p10 p01 `shouldEql` ccw,
    orient p00 p01 p10 `shouldEql` cw,
    orient p00 p10 p10' `shouldEql` 0,
    orient p00 (-1, 1) p11' `shouldEql` ccw,
    orient p00 p10 p00 `shouldEql` 0,

    angleBetween p00 p00 p00 `shouldEql` 0,
    angleBetween p01 p00 p00 `shouldEql` 0,
    angleBetween p00 p01 p00 `shouldEql` 0,
    angleBetween p00 p00 p01 `shouldEql` 0,

    angleBetween p00 p10 p01 `shouldEql` 90,
    angleBetween p00 p01 p10 `shouldEql` 270,

    angleBetween p00 p01 p01' `shouldEql` 180,
    angleBetween p00 p01' p01 `shouldEql` 180,

    angleBetween p00 p10' p10 `shouldEql` 180,
    angleBetween p00 p10 p10' `shouldEql` 180,

    angleBetween p01 p00 p10 `shouldEql` 45,
    angleBetween p01 p10 p00 `shouldEql` (360 - 45),

    angleBetween p00 (-1, 1) p11' `shouldEql` 90,
    angleBetween p00 p11' (-1, 1) `shouldEql` 270,

    "done"

    ]
