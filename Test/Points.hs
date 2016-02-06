module Test.Points where

import qualified Src.Points as Points

tests :: [String]
tests = [
    Points.test `shouldEql` "bla"
    ]

shouldEql :: (Eq a, Show a) => a -> a -> String
shouldEql a b
    | a == b = "ok"
    | otherwise = error $ show a ++ " not equal " ++ show b
