{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.List ( map, words, intercalate, tail, init )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import System.IO.Unsafe

circularArrayRotation a k n = map (\q -> a!!((q - k) `mod` n)) 

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

    let k = read (firstMultipleInput !! 1) :: Int

    let q = read (firstMultipleInput !! 2) :: Int

    aTemp <- getLine

    let a = map (read :: String -> Int) . words $ rstrip aTemp

    queriesTemp <- readMultipleLinesAsStringArray q
    let queries = map (read :: String -> Int) queriesTemp

    let result = circularArrayRotation a k n queries

    hPutStrLn fptr $ intercalate "\n" $ map show result

    hFlush fptr
    hClose fptr
