{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( filter, map, length, words, head, tail, subsequences )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

getDivisibleSum :: Int -> Int -> [Int] -> Int
getDivisibleSum k x ar = length $ filter ( \v -> (v + x) `mod` k == 0) ar

divisibleSumPairs :: Int -> [Int] -> Int
divisibleSumPairs k ar = case ar of
    [] -> 0
    (x:xs) -> getDivisibleSum k x xs + divisibleSumPairs k xs
    
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    arTemp <- getLine

    let ar = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arTemp

    let result = divisibleSumPairs k ar
    print result

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
 