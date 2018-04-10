#!/usr/bin/env stack

import Data.List.Split
import Data.Char
import Data.List
import System.IO

slice xs from to = map (xs !!) [from..to]

getNum :: String -> Integer
getNum (x:y:_) = toInteger $ 10 * (Data.Char.digitToInt x) + (Data.Char.digitToInt y)

takeN :: Int -> [Integer] -> [Integer]
takeN n x |
    (length x) < n = [0]
takeN n (x:xs) = [foldr (*) x x_n] ++ takeN n xs
    where
        x_n = map (xs !!) [0..(n-2)]

gridMax ::[[Integer]] -> Integer
gridMax g = maximum $ map maximum g

orientMax ::[[Integer]] -> Integer
orientMax g = gridMax $ map (takeN 4) g

indent :: [Integer] -> Int -> [Integer]
indent x n = (replicate n 0) ++ x

shift :: [[Integer]] -> Int -> [Integer]
shift m n = indent (m !! n) n 

tilt :: [[Integer]] -> [[Integer]]
tilt m = map (shift m) [0.. ((length m) - 1)]

main :: IO ()
main = do
    handle <- openFile "data/Problem11.txt" ReadMode
    contents <- hGetContents handle
    let g1 = splitOn "\n" contents
    let g2 = map (g1 !!) [0..((length g1) - 2)]
    let g3 = map (splitOn " ") g2
    let grid = map (map getNum) g3
    let grid_t = transpose grid
    let grid_d1 = transpose (tilt grid)
    let grid_d2 = transpose (tilt (reverse grid))
    let r = [orientMax grid,orientMax grid_t, orientMax grid_d1, orientMax grid_d2]
    putStrLn $ show $ maximum r
