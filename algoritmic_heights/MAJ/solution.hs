import Data.List
import Control.Monad

maj :: (Num b, Ord b) => Int -> [b] -> b
maj n list = if fst t > n `quot` 2 then snd t else -1
    where
        t = (maximum.count.group.sort) list
        l = length list
        count = map (\l -> (length l, head l))

-- IO
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
    line <- getLine
    return $ map rd $ words line

main :: IO ()
main = do
    [k,n] <- getInts
    lists <- replicateM k getInts
    putStrLn $ unwords $ map (show . maj n) lists
