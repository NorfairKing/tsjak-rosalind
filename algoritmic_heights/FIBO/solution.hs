-- All fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

-- The n-th fibonacci number
solve :: Int -> Integer
solve n = fibs !! n

-- IO
rd :: String -> Int
rd x = read x :: Int

getInt :: IO Int
getInt = do
    line <- getLine
    return $ rd line

main :: IO ()
main = do
    i <- getInt
    putStrLn $ show $ solve i
    
