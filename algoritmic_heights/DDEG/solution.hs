import Control.Monad

degree :: Eq a => [(a, a)] -> a -> Int
degree eds x = (sum . map (count x)) eds

count :: Eq a => a -> (a, a) -> Int
count x (y,z) = fromEnum (x == y) + fromEnum (x == z)

contains :: Eq a => a -> (a, a) -> Bool
contains x (y,z) = x == y || x == z

other :: Eq a => a -> (a, a) -> a
other x (y,z) | contains x (y,z) = if x == y then z else y
other _ (_,_) | otherwise = error "wut"

neighborss :: (Enum a, Eq a, Num a) => a -> [(a, a)] -> [[a]]
neighborss n eds = map (neighbors eds) [1..n]

neighbors :: Eq a => [(a, a)] -> a -> [a]
neighbors eds x  = (map (other x) eds')
    where eds' = filter (contains x) eds

solve :: (Enum a, Eq a, Num a) => a -> [(a, a)] -> [Int]
solve n eds = map (sum . map (degree eds)) ns
    where ns = neighborss n eds

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
    putStrLn $ unwords . map show $ solve n edges
    where
        tuplify [x,y] = (x,y)  
        tuplify _ = error "wut"
