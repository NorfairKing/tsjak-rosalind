import Control.Monad
import Data.Maybe
import qualified Data.Map.Lazy as M

-- The famous three sum program, solved with an O(logN) lookup structure.
threeSum :: [Int] -> Maybe (Int, Int, Int)
threeSum xs 
    = mHead $ filter isJust idc
    where 
        idc = map (\(z, xi, yi) -> case M.lookup (-z) (indexMap xs) of
                                        Just zi -> Just (xi, yi, zi)
                                        Nothing -> Nothing     ) poss
        
        poss = [ (x+y,xi,yi) | (x,xi) <- xsl, (y,yi) <- xsl ]
        xsl = xsList xs

-- The first Just element from a list, or Nothing.
mHead :: [Maybe a] -> Maybe a
mHead [] = Nothing
mHead (Nothing:xs) = mHead xs
mHead (mx@(Just x):_) = mx

-- A map of elements to their indices in the original lest.
indexMap :: [Int] -> M.Map Int Int
indexMap xs = M.fromList $ xsList xs

-- A list of the elements with their indices
xsList :: [Int] -> [(Int, Int)]
xsList xs = zip xs [1..]


-- IO
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
    line <- getLine
    return $ map rd $ words line

showResult :: Maybe (Int, Int, Int) -> String
showResult Nothing = "-1"
showResult (Just (xi,yi,zi)) = (unwords . map show) [xi,yi,zi]

main :: IO ()
main = do
    [k, _] <- getInts
    xss <- replicateM k getInts
    putStrLn $ init $ unlines $ map (showResult . threeSum) xss
