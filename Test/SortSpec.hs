module Main where

import Sort
import Support
import Data.Point

c :: Point
c = Point 0 0

main :: IO ()
main = do
    putStrLn $
        sortClockwise [] c `shouldEql` []
    putStrLn $
        sortClockwise [c] c `shouldEql` [c]
