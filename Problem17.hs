#!/usr/bin/env stack
--import Data.HashMap.Strict as M
import System.IO
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe

tpl :: [a] -> (a,a)
tpl [x,y] = (x,y)

getNameLen :: String -> Map.Map String String -> Int
getNameLen n d
    | isNothing (Map.lookup n d) = 0
getNameLen n d = length $ fromJust (Map.lookup n d)

getName n d
    | not (isNothing (Map.lookup n d)) = fromJust (Map.lookup n d)
    | n == "1000" = "onethousand"
    | (length n) > 2 =  gn (take 1 n) ++ "hundredand" ++ (getName (drop 1 n) d)
    | (length n) > 1 && (take 1 n) == "0" = (getName (drop 1 n) d)
    | (length n) > 1 = gn ((take 1 n) ++ "0") ++ (getName (drop 1 n) d)
    where gn m = getName m d

main :: IO ()
main = do
    let nums = map show [1..1000]
    handle <- openFile "data/Problem17.txt" ReadMode
    contents <- hGetContents handle
    let g1 = splitOn "\n" contents
    let g2 = map (g1 !!) [0..((length g1) - 2)]
    let g3 = map (tpl . splitOn "\t") g2
    let g4 = Map.fromList g3
    let g x = getName x g4
    putStrLn $ show $ length $ concatMap g $ nums 
