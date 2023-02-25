{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words, intercalate )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Debug.Trace
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

serviceLane n [] width = []
serviceLane n ([start, end]:xs) width = minimum (take (end - start + 1) $ drop start width) : serviceLane n xs width


lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

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

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let t = read (firstMultipleInput !! 1) :: Int

    widthTemp <- getLine

    let width = map (read :: String -> Int) . words $ rstrip widthTemp

    casesTemp <- readMultipleLinesAsStringArray t
    let cases = map (map (read :: String -> Int) . words . rstrip) casesTemp

    let result = serviceLane n cases width

    print result

    hPutStrLn fptr $ intercalate "\n" $ map show result

    hFlush fptr
    hClose fptr
