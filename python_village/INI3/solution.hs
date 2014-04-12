main :: IO ()
main = do
    line <- getLine
    nrs <- getLine
    let [a, b, c, d] = map read $ words nrs
    putStrLn $ (take (b-a+1) . drop a ) line
    putStrLn $ (take (d-c+1) . drop c ) line
