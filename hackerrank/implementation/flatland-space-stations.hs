{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Data.List ( map, sort )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

flatlandSpaceStations :: [Int] -> [Int]
flatlandSpaceStations [] = []
flatlandSpaceStations [x] = []
flatlandSpaceStations (x:y:xs) = (y - x) : flatlandSpaceStations (y:xs)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nmTemp <- getLine
    let nm = words nmTemp

    let n = read (nm !! 0) :: Int

    let m = read (nm !! 1) :: Int

    cTemp <- getLine

    let c = sort . map (read :: String -> Int) . words $ cTemp

    let result = maximum $ head c : map (`div` 2) (flatlandSpaceStations c) ++ [n - 1 - last c]

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
