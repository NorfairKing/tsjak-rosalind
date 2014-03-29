solve :: Eq b => [b] -> [b] -> Int
solve l1 l2 = sum $ map fromEnum $ zipWith (/=) l1 l2

main :: IO ()
main = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ show $ solve l1 l2
