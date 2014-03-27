import Data.List

-- This works in O(n) time.
solve :: String -> String
solve []       = []
solve ('T':xs) = 'U' : solve xs
solve (x:xs)   =  x  : solve xs


main :: IO ()
main = do
    line <- getLine
    putStrLn $ solve line
