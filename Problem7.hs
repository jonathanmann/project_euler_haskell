module Problem7 where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

getLowPrimes :: Int -> [Int]
getLowPrimes k = [x | x <- [2..isqrt k], mod k x == 0, isPrime(x)]

isPrime :: Int -> Bool
isPrime k = null $ getLowPrimes k

getHighPrime :: Int -> Int -> Int
getHighPrime k x = if mod k x == 0 && isPrime(r) then r else 0
    where r = div k x

getPrimes :: Int -> [Int]
getPrimes k = [(getHighPrime k x) | x <- lowPrimes] ++ lowPrimes
    where lowPrimes = getLowPrimes k

getLargestPrime :: Int -> Int
getLargestPrime k  = maximum (getPrimes k)

main :: IO ()
main = putStrLn $ show $ getLargestPrime 600851475143
