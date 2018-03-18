module Problem7 where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

getLowPrimes :: Int -> [Int]
getLowPrimes k = [x | x <- [2..isqrt k], mod k x == 0, isPrime(x)]

isPrime :: Int -> Bool
isPrime k = null $ getLowPrimes k

getNthPrime :: Int -> Int
getNthPrime n = allPrimes !! (n - 1)
    where allPrimes = [x | x <- [2..], isPrime(x)]

main :: IO ()
main = putStrLn $ show $ getNthPrime 10001
