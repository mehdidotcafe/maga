module Main where

import Control.Monad
import Data.List (map, words)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

initialE = 100

jumpingOnClouds c k e i
  | i == 0 && e /= initialE = e
  | c!!nextI == 1 = jumpingOnClouds c k (e - 3) nextI
  | otherwise = jumpingOnClouds c k (e - 1) nextI
  where
    nextI = (i + k) `mod` length c
    nextC = c !! (i + k)

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

    nkTemp <- getLine
    let nk = words nkTemp

    let n = read (nk !! 0) :: Int

    let k = read (nk !! 1) :: Int

    cTemp <- getLine

    let c = map (read :: String -> Int) . words $ cTemp

    let result = jumpingOnClouds c k initialE 0

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
