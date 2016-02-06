module Main where

import qualified Test.Points as Points
import qualified Test.Sort as Sort

main :: IO ()
main = putStrLn $ unlines
    Points.tests : Sort.tests : []
