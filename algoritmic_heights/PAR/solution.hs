partition :: Ord a => [a] -> [a]
partition [] = []
partition (x:xs) = filter (<=x) xs ++ [x] ++ filter (>x) xs


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
    putStrLn $ unwords $ map show $ partition xs
