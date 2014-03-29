-- The infinite modified fibonacci sequence
rec :: Num a => a -> [a]
rec k = 0 : 1 : zipWith rep r (tail r)
    where
        r = rec k
        rep x y = k*x + y

-- Take the n-th element of the sexuence
solve :: Num a => Int -> a -> a
solve n k = (rec k) !! n

-- I/O
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
        line <- getLine
        return (map rd $ words line)

main :: IO ()
main = do
    [n, k] <- getInts
    putStrLn $ show $ solve n k
