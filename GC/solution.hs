import Data.List
import Text.Printf

gc :: [Char] -> Double
gc base = 100 * (fromIntegral lg) / (fromIntegral l)
    where
        l = length base
        lg = length $ filter (\x -> x=='G'||x=='C') base

-- I/O
parseFastas :: [[Char]] -> [([Char], [Char])]
parseFastas []         = []
parseFastas (l1:ls) = (id, concat bases) : (parseFastas lx)
    where
        id = tail l1 -- Tail because the ">" character is not part of the ID.
        bases = takeWhile (not.isId) ls
        lx = drop (length bases) ls
        isId ('>':_) = True
        isId _       = False        
    
main :: IO ()
main = do
    s <- getContents
    let fs = parseFastas $ lines s
    let (m, id) = maximum [ (gc base, i) | (i, base) <- fs ]
    putStrLn id
    printf "%.6f\n" m  
