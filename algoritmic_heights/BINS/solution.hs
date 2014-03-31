-- The ugliest binary search code...
bins :: (Enum b, Num b, Ord a) => [a] -> a -> b
bins array = search (zip array [1..])
    where
        search []  e = -1
        search [a] e = if fst a == e
                        then snd a
                        else (-1)
        search as e
            | fst m == e = snd m
            | fst m > e = search firstHalf e
            | otherwise = search secondHalf e
            where 
                firstHalf = take n' as
                (m : secondHalf) = drop n' as
                n' = length as `quot` 2        

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
    _ <- getInt
    as <- getInts
    ks <- getInts
    putStrLn $ unwords $ map (show . bins as) ks   
