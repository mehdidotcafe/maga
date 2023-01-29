{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List (filter, map)
import System.Environment ( getEnv )
import System.IO

getMoneySpent keyboards drives b
  | null sums = -1
  | otherwise = maximum sums 
  where
  sums = [ x + y | x <- keyboards, y <- drives, x + y <= b ]
  
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

    bnmTemp <- getLine
    let bnm = words bnmTemp

    let b = read (bnm !! 0) :: Int

    let n = read (bnm !! 1) :: Int

    let m = read (bnm !! 2) :: Int

    keyboardsTemp <- getLine

    let keyboards = map (read :: String -> Int) . words $ keyboardsTemp

    drivesTemp <- getLine

    let drives = map (read :: String -> Int) . words $ drivesTemp
    let moneySpent = getMoneySpent keyboards drives b

    hPutStrLn fptr $ show moneySpent

    hFlush fptr
    hClose fptr
