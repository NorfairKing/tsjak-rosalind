import Data.List

-- This works in O(nlogn) time, but has nice code.
solve :: Ord a => [a] -> String
solve line = (unwords . (map (show . length)) . group . sort) line 

-- This works in linear time, but is pretty ugly.
solve' :: [Char] -> String
solve' line = (unwords . (map show)) $ go line [0,0,0,0]
    where
        go [] ll = ll
        go ('A':xs) [a,c,g,t] = go xs [(a+1),c,g,t]
        go ('C':xs) [a,c,g,t] = go xs [a,(c+1),g,t] 
        go ('G':xs) [a,c,g,t] = go xs [a,c,(g+1),t] 
        go ('T':xs) [a,c,g,t] = go xs [a,c,g,(t+1)]
        go _ _ = error "Input line is not a DNA string."        

main :: IO ()
main = do
    line <- getLine
    putStrLn $ solve' line
