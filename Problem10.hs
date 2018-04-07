#!/usr/bin/env stack

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime k = null $ [x | x <- [2..isqrt k], mod k x == 0]

main :: IO ()
main = do
    putStrLn $ show $ sum $ [x | x <- [2..2000000], isPrime x]
