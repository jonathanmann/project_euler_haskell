#!/usr/bin/env stack
import System.IO
import Data.List.Split
import Data.Char

getDig :: [Char] -> Int
getDig [a,b] = 10 * (Data.Char.digitToInt a) + (Data.Char.digitToInt b)

get_row_path :: [Int] -> [Int]
get_row_path (x:y)
    | y == [] = [x]
get_row_path (x:y:z)
    | z == [] = [maximum [x,y]]
get_row_path (x:y:z) = [maximum [x,y]] ++ get_row_path (y:z)

get_path_max t p i
    | i < 0 = p
    | i == (sz - 1) = get_path_max t (get_row_path currRow) (i - 1)
    | otherwise =  get_path_max t newRow (i - 1)
    where
        sz = (length t)
        currRow = t !! i
        newRow = get_row_path $ zipWith (+) currRow p
    
main :: IO ()
main = do
    handle <- openFile "data/Problem18.txt" ReadMode
    contents <- hGetContents handle
    let g1 = splitOn "\n" contents
    let g2 = map (g1 !!) [0..((length g1) - 2)]
    let g3 = map (map getDig) (map (splitOn " ") g2)
    putStrLn $ show $ (get_path_max g3 [] ((length g3) - 1)) !! 0
