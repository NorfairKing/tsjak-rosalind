import Data.List

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

main :: IO ()
main = do
    cs <- getContents
    let edges = sort $ map (tuplify . map rd . words) . tail . lines$ cs 
    putStrLn $ (unwords . map show . degrees) edges
    where
        tuplify [x,y] = (x,y)  
        tuplify _ = error "wut"
