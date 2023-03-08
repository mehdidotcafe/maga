{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Debug.Trace (trace)


workbook n k cp [] = 0
workbook n k cp ([]:xs) = workbook n k (cp + 1) xs
workbook n k cp ((y:ys):xs) = sc + workbook n k ncp (ys:xs)
  where
    sc | y == cp = 1 | otherwise = 0
    ncp | y `mod` k == 0 && ys /= [] = cp + 1 | otherwise = cp

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    arrTemp <- getLine

    let arr = map ((\x -> [1..x]) . (read :: String -> Int)) . words $ rstrip arrTemp

    let result = workbook n k 1 arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
