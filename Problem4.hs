module Problem4 where

getRange :: Int -> Int -> [Int]
getRange p n = reverse [x | x <- [mn^p..mx^p]]
    where
        mn = 10^(n - 1)
        mx = 10^n - 1

travList :: Int -> [Int] -> [Int]
travList n [] = []
travList n (x:xs) = if (isPalindrome x) && (hasMinDigFac n x) then [x] else travList n xs

isPalindrome :: Int -> Bool
isPalindrome a = a' == reverse(a')
    where a' = show a

hasMinDig :: Int -> Int -> Bool
hasMinDig mn n = length(show n) == mn 

hasMinDigFac :: Int -> Int -> Bool
hasMinDigFac mn n = not $ null [y | y <- digs, mod n y == 0, hasMinDig mn (div n y) ]
    where digs = reverse [x | x <- getRange 1 mn]

main :: IO ()
main = putStrLn $ show $ travList 3 $ getRange 2 3

--main = putStrLn $ show $ getBestPal 3 
--main = putStrLn $ show $ (getBestPalList 3) !! 0

--Extras
--getBestPal :: Int -> Int
--getBestPal n  = [x | x <- (getRange 2 n),isPalindrome x,hasMinDigFac n x] !! 0
--getBestPalList :: Int -> [Int]
--getBestPalList n  = [x | x <- (getRange 2 n),isPalindrome x,hasMinDigFac n x]
