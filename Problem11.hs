#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split
import Data.Char
import Data.List
import System.IO
--import Control.Monad

slice xs from to = map (xs !!) [from..to]

{-
sliceStr :: String -> Int -> Int -> String
sliceStr str from to = map (str !!) [from..to]
treatList :: String -> Int -> Int -> [Int]
treatList inStr start end |
    end > 999 = [] 
treatList inStr start end = [product (getDig (sliceStr inStr start end))] ++ treatList inStr (start + 1) (end + 1)
-}
getNum :: String -> Int
getNum (x:y:_) = 10 * (Data.Char.digitToInt x) + (Data.Char.digitToInt y)

takeN :: Int -> [Int] -> [Int]
takeN n x |
    (length x) < n = []
takeN n (x:xs) = [foldr (*) x x_n] ++ takeN n xs
    where
        x_n = map (xs !!) [0..(n-2)]

gridMax ::[[Int]] -> Int
gridMax g = maximum $ map maximum g

orientMax ::[[Int]] -> Int
orientMax g = gridMax $ map (takeN 4) g

main :: IO ()
main = do
    handle <- openFile "data/Problem11.txt" ReadMode
    contents <- hGetContents handle
    let g1 = splitOn "\n" contents
    let g2 = map (g1 !!) [0..((length g1) - 2)]
    let g3 = map (splitOn " ") g2
    let g4 = map (map getNum) g3
    putStrLn $ show $ g4
    putStrLn $ show $ orientMax g4
