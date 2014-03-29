import Data.List

solve :: String -> String
solve = (map complement) . reverse

complement :: Char -> Char
complement 'A' = 'T'
complement 'C' = 'G'
complement 'G' = 'C'
complement 'T' = 'A'
complement _ = error "Character is not a DNA base"

main :: IO ()
main = do
    line <- getLine
    putStrLn $ solve line