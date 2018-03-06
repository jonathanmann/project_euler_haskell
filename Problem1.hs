module Problem1 where

getSum :: Integer -> Integer
getSum n = n * (n + 1) `div` 2

getDivSum :: Integer -> Integer -> Integer
getDivSum n x = getSum((n - 1) `div` x) * x

getComboSum :: Integer -> Integer -> Integer -> Integer
getComboSum n x y = getDivSum n x + getDivSum n y - getDivSum n (x * y)

main :: IO ()
main = putStrLn $ show $ getComboSum 1000 3 5
