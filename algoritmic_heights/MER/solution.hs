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
    _ <- getInt
    ys <- getInts
    putStrLn $ unwords $ map show $ merge xs ys
