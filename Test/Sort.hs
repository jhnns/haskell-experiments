module Test.Sort where

import Src.Sort
import Test.Support

tests :: [String]
tests = [
    (sortClockwise []) `shouldEql` []
    ]
