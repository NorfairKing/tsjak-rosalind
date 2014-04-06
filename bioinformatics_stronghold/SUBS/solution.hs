solve :: Eq a => [a] -> [a] -> [Int]
solve line motif = zipSelect $ map (== motif) chunks
    where
        chunks = ps n line
        n = length motif

zipSelect :: [Bool] -> [Int]
zipSelect bs = go [1..n] bs
    where
        n = length bs
        go [] [] = []
        go (n:nn) (b:bs) 
            = if b 
            then n : go nn bs
            else go nn bs



ps :: Int -> [a] -> [[a]]
ps _ [] = [[]]
ps p l | length l < p = [[]]
ps p l@(_:xs) = take p l : ps p xs

main :: IO ()
main = do
    code <- getLine
    motif <- getLine
    putStrLn $ unwords $ map show $ solve code motif
