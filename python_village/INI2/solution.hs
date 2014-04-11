main :: IO ()
main = do
    line <- getLine
    let [a, b] = map read $ words line
    putStrLn $ show (a^2+b^2)
