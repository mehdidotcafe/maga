{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

getNumberDividedBy :: [Int] -> [Int] -> [Int]
getNumberDividedBy dividers toTest = Data.List.filter (\n -> Data.List.all (\t -> (n `mod` t) == 0) dividers) toTest

getNumberDividing :: [Int] -> [Int] -> [Int]
getNumberDividing toDivide toTest = Data.List.filter (\n -> Data.List.all (\t -> (t `mod` n) == 0) toDivide) toTest

getTotalX :: [Int] -> [Int] -> Int
getTotalX a b = Data.List.length . getNumberDividing b $ getNumberDividedBy a [Data.List.last a .. Data.List.head b]

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    brrTemp <- getLine

    let brr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip brrTemp

    let total = getTotalX arr brr

    hPutStrLn fptr $ show total

    hFlush fptr
    hClose fptr
