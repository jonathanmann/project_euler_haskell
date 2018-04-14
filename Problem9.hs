#!/usr/bin/env stack

getTProd n = [(a*b*c) 
    | c <- x, b <- x, a <- x, a < b, b < c, a^2 + b^2 == c^2, a + b + c == n] !! 0
    where x = [0..n]

main :: IO ()
main = do
    putStrLn $ show $ getTProd 1000
