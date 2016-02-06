
-- http://stackoverflow.com/questions/3467279/how-to-create-a-polyvariadic-haskell-function
-- http://chris-taylor.github.io/blog/2013/03/01/how-haskell-printf-works/
class PrintfType t where
    spr :: String -> [UPrintf] -> t

instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \a -> spr fmts (toUPrintf a : args)

equal :: (Eq a, Show a) => a -> a -> [Char]
equal a b
    | a == b = "ok"
    | otherwise = show a ++ " not equal " ++ show b
