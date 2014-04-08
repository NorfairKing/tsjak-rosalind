import Control.Monad

twoSum :: [Int] -> String
twoSum xs = if length ll >= 1
            then (unwords . map show) $ head ll
            else "-1"
    where
        ll = indices $ zip [1..] xs
        indices xs = [ [p,q] | (p,x) <- xs, (q,y) <- xs, x == -y, p < q ] 


-- IO
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
    line <- getLine
    return $ map rd $ words line

main :: IO ()
main = do
    [k, _] <- getInts
    xss <- replicateM k getInts
    putStrLn $ init $ unlines $ map twoSum xss
