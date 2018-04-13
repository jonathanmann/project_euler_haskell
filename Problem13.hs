#!/usr/bin/env stack

import Data.List.Split
import Data.Char
import Data.List
import System.IO

slice xs from to = map (xs !!) [from..to]

getNum :: String -> Integer
getNum i = read i :: Integer

takeN :: Int -> [Integer] -> [Integer]
takeN n x |
    (length x) < n = [0]
takeN n (x:xs) = [foldr (*) x x_n] ++ takeN n xs
    where
        x_n = map (xs !!) [0..(n-2)]

--firstN :: Int -> String -> String 
firstN n x = slice (show n) 0 (n - 1)

sm [] = 1
sm (x:xs) = x + sm xs

main :: IO ()
main = do
    handle <- openFile "data/Problem13.txt" ReadMode
    contents <- hGetContents handle
    let g1 = splitOn "\n" contents
    let g2 = map (g1 !!) [0..((length g1) - 2)]
    let g3 = map getNum g2
    --putStrLn $ show $ slice (show (g3 !! 0)) 0 9
    let t = sm g3
    putStrLn $ show $ slice (show t) 0 9
    {-
    let g3 = map (splitOn " ") g2
    let grid = map (map getNum) g3
    let grid_t = transpose grid
    let grid_d1 = transpose (tilt grid)
    let grid_d2 = transpose (tilt (reverse grid))
    let r = [orientMax grid,orientMax grid_t, orientMax grid_d1, orientMax grid_d2]
    -}
