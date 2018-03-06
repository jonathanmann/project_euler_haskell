module Problem2 where
import Data.List (filter)

getFibMap :: Int -> Integer
getFibMap = (map getFib [0..] !!) 
    where 
        getFib 0 = 1
        getFib 1 = 1
        getFib n = getFibMap(n-1) + getFibMap(n-2)

main :: IO ()
main = putStrLn $ show $ sum(filter even (takeWhile (<4000000) (map getFibMap [0..])))
