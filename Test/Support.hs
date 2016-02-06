module Support where

shouldEql :: (Eq a, Show a) => a -> a -> String
shouldEql a b
    | a == b = "ok"
    | otherwise = error $ show a ++ " not equal " ++ show b
