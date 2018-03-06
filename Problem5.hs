module Problem5 where

groupLCM :: Int -> [Int] -> Int
groupLCM a [] = a
groupLCM a (x:xs) = groupLCM (lcm a x) xs

main :: IO ()
main = putStrLn $ show $ groupLCM 2 [3..20]
