gc :: Fractional a => [Char] -> a
gc base = (fromIntegral lg) / (fromIntegral l)
    where
        l = length base
        lg = length $ filter (\x -> x=='G'||x=='C') base

-- I/O
parseFastas :: [[a]] -> [([a], [a])]
parseFastas []         = []
parseFastas (l1:l2:ls) = (id, bases) : (parseFastas ls)
    where
        id = tail l1 -- Tail because the ">" character is not part of the ID.
        bases = l2

main :: IO ()
main = do
    s <- getContents
    let fs = parseFastas $ lines s
    let (m, id) = maximum [ (gc base, i) | (i, base) <- fs ]
    putStrLn id
    putStrLn $ show m  
