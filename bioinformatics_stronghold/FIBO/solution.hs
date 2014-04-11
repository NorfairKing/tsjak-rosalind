fibs' m = 0 : 1 : [(sum $ take (m-1) $ fibs' m)]

solve m n = fibs' m !! (n-1)

-- I/O
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
        line <- getLine
        return (map rd $ words line)

main :: IO ()
main = do
    [n, m] <- getInts
    putStrLn $ show $ solve m n
