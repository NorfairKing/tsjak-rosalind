import Control.Monad

degrees :: (Enum a, Num a, Ord a) => [(a, a)] -> [Int]
degrees eds = [ (sum . map (count x)) eds | x <- [1..n] ]
    where
        n = getN eds

getN :: Ord a => [(a, a)] -> a
getN eds = maximum [ max x y | (x,y) <- eds ]

count :: Eq a => a -> (a, a) -> Int
count x (y,z) = fromEnum (x == y) + fromEnum (x == z)

-- IO
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
    line <- getLine
    return $ map rd $ words line

main :: IO ()
main = do
    [n,e] <- getInts
    eds <- replicateM e getInts
    let edges = map tuplify eds 
    putStrLn $ (unwords . map show . degrees) edges
    where
        tuplify [x,y] = (x,y)  
        tuplify _ = error "wut"
