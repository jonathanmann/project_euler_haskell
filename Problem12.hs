#!/usr/bin/env stack

fetchTriNum :: Int -> Int
fetchTriNum = (map getTriNum [0 ..] !!)
    where 
        getTriNum 1 = 1
        getTriNum n = n + fetchTriNum (n - 1)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

getLowFactors :: Int -> [Int]
getLowFactors k = [x | x <- [1..isqrt k], mod k x == 0]

getHighFactors :: Int -> Int -> Int
getHighFactors k x = if mod k x == 0 then r else 0
    where r = div k x

getFactors :: Int -> [Int]
getFactors k = [(getHighFactors k x) | x <- lowFactors] ++ lowFactors
    where lowFactors = getLowFactors k

factorLen n = length $ getFactors n

findFac :: Int -> Int
findFac i = 
    if ((factorLen n) < 500)
        then findFac (i + 1)
        else n
    where n = fetchTriNum i

main :: IO ()
main = do
    putStrLn $ show $ findFac 2
