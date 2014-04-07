inversions :: Ord a => [a] -> Int
inversions ll = sum $ map lessers $ tails ll

lessers :: Ord a => [a] -> Int
lessers [] = 0
lessers [_] = 0
lessers (x:xs) = sum $ map (fromEnum . (< x)) xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails ll= ll : tails ll'
    where ll' = tail ll

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
    ll <- getInts
    print $ inversions ll   
