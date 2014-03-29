import Data.List

solve :: [Char] -> Int -> String
solve str k = (unwords . sort . (map snd) . (takeWhile (\x -> fst x == fst m))) res 
    where 
        m = maximum res
        res = (reverse.sort) [(frec ks ki, ki) | ki <- nub ks]
        ks = kmers k str

-- Count the frequency of an element in a list.
frec :: Eq a => [a] -> a -> Int
frec ks ki =  sum $ map (fromEnum . (==ki)) ks

-- Get all k-long sublists from a list
kmers :: Int -> [a] -> [[a]]
kmers _ [] = []
kmers k str@(_:ll) | (length str) < k = []
                   | otherwise        = (take k str) : (kmers k ll)

rd :: String -> Int
rd x = read x :: Int

getInt :: IO Int
getInt = do
    line <- getLine
    return $rd line

main :: IO ()
main = do
    line <- getLine
    k <- getInt
    putStrLn $ solve line k
