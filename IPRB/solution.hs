import Data.List

solve :: Fractional a => Int -> Int -> Int -> a
solve i1 i2 i3 = (fromIntegral success) / (fromIntegral total)
    where
        total = ((ii)*(ii-1)*4)
        ii = i1+i2+i3
        success = sum $ map (sum.(map fromEnum).grade) ll
        ll = [ ((snd a), (snd b)) | a <- l, b <- l, a /= b]
        l = zip [1..] $
            replicate i1 [True,True] ++ 
            replicate i2 [True,False] ++ 
            replicate i3 [False,False]

-- Gives all possible phenotypes given two partners
grade :: ([Bool],[Bool]) -> [Bool]
grade (l1,l2) = [ a || b | a <- l1, b <- l2]

-- IO
rd :: String -> Int
rd x = read x :: Int

getInts :: IO [Int]
getInts = do
        line <- getLine
        let ints = map rd $ words line
        return ints

main :: IO ()
main = do
    [i1,i2,i3] <- getInts
    putStrLn $ show $ solve i1 i2 i3
