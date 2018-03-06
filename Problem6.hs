module Problem6 where

naiveDiff :: Int -> Int
naiveDiff n = sum([1..n])^2 - sum([x^2|x <- [1..n]])

smartDiff :: Int -> Int
smartDiff n = (div (n * (n+1)) 2)^2 - (div (n * (n+1) * (2*n + 1)) 6) 

main :: IO ()
main = putStrLn $ show $ smartDiff 100
