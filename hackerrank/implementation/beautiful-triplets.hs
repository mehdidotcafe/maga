{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Array (listArray, (!))

beautifulTriplets d n a (i, j, k)
    | i >= (n - 2) = 0
    | j >= (n - 1) = beautifulTriplets d n a (i + 1, i + 2, i + 3)
    | k == n = beautifulTriplets d n a (i, j + 1, j + 2)
    | a!j - a!i == d && a!k - a!j == d = 1 + beautifulTriplets d n a (i + 1, i + 2, i + 3)
    | a!j - a!i == d = beautifulTriplets d n a (i, j, k + 1)
    | otherwise = beautifulTriplets d n a (i, j + 1, j + 2)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let d = read (firstMultipleInput !! 1) :: Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ rstrip arrTemp

    let result = beautifulTriplets d n (listArray (0, n) arr) (0, 1, 2)

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
