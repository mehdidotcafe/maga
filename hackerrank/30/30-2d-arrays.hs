{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )

side = 6

hourglass = [
  0, 1, 2, side + 1, 2 * side, 2 * side + 1, 2 * side + 2
  ]

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

highestHourglass i arr
  | i `mod` side >= side - 2 = highestHourglass (i + 2) arr
  | i < side * side - 2 * side = max s (highestHourglass (i + 1) arr) 
  | otherwise = minBound
  where
    s = sum $ map (\hg -> arr!!((i + hg) `div` side)!!((i + hg) `mod` side)) hourglass

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do

    arrTemp <- readMultipleLinesAsStringArray 6
    let arr = map (map (read :: String -> Int) . words . rstrip) arrTemp
  
    putStrLn $ show $ highestHourglass 0 arr
