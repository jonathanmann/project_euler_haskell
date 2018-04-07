#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Data.List.Split
import Data.Char
import Data.List

slice xs from to = map (xs !!) [from..to]

removeNonNumbers :: String -> String
removeNonNumbers = filter Data.Char.isDigit

sliceStr :: String -> Int -> Int -> String
sliceStr str from to = map (str !!) [from..to]

getDig :: String -> [Int]
getDig [] = []
getDig (x:xs) = [Data.Char.digitToInt x] ++ getDig(xs)

treatList :: String -> Int -> Int -> [Int]
treatList inStr start end |
    end > 999 = [] 
treatList inStr start end = [product (getDig (sliceStr inStr start end))] ++ treatList inStr (start + 1) (end + 1)

main :: IO ()
main = do
    response <- httpLBS "https://projecteuler.net/problem=8"
    let body =  show $ getResponseBody response
    let section = slice (splitOn "\\n" body) 50 70
    let numStr = removeNonNumbers $ unwords $ map removeNonNumbers section    
    let shortStr = sliceStr numStr 0 12
    putStrLn $ show $ length numStr
    putStrLn $ show $ product (getDig shortStr)
    putStrLn $ show $ maximum $ treatList numStr 0 12
