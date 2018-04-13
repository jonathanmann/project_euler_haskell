#!/usr/bin/env stack
import Data.List

collatz :: Integer -> Integer -> Integer
collatz n c
    | n < 2 = c + 1
    | (mod n 2 == 0) = collatz (div n 2) (c + 1)
collatz n c = collatz (3 * n + 1) (c + 1)

ctz :: Integer -> Integer
ctz n = collatz n 0

main :: IO ()
main = do
    let x = map ctz [0..999999]  
    let m = maximum x
    putStrLn $ show $ elemIndex m x
