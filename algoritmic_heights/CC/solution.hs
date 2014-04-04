import Control.Monad
import Data.List

connectedComponents :: Int -> [(Int,Int)] -> Int
connectedComponents n es = (length.nub) $ foldr unite [1..n] es

unite :: (Int, Int) -> [Int] -> [Int]
unite (x,y) list = go list xid yid
    where 
        go [] _ _ = []
        go (l:ls) xi yi = if l == xi
                                then yi : go ls xi yi 
                                else l  : go ls xi yi
        xid = list !! (x-1)
        yid = list !! (y-1)

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
    print $ connectedComponents n edges
    where
        tuplify [x,y] = (x,y)  
        tuplify _ = error "wut"
