#!/usr/bin/env stack
--import System.IO
--import Data.List.Split
import Data.Char

factorial :: Integer -> Integer
factorial n = product [1..n]


main :: IO ()
main = do
    let char_digits = show $ factorial 100
    let digits =  map Data.Char.digitToInt char_digits
    putStrLn $ show $ sum $ digits
