mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge firstHalf secondHalf 
    where
        firstHalf  = mergesort $ take n xs
        secondHalf = mergesort $ drop n xs
        n = length xs `quot` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- IO
rd :: String -> Int
rd x = read x :: Int

getInt :: IO Int
getInt = do
    line <- getLine
    return $ rd line

getInts :: IO [Int]
getInts = do
    line <- getLine
    return $ map rd $ words line

main :: IO ()
main = do
    _ <- getInt
    xs <- getInts
    putStrLn $ unwords $ map show $ mergesort xs
