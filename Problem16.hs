#!/usr/bin/env stack

import Data.Char

sumDigits = sum . map Data.Char.digitToInt . show

main :: IO ()
main = do
    putStrLn $ show $ sumDigits $ 2^1000
