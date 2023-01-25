{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

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

isValidWay :: [Int] -> Int -> Int -> Int
isValidWay s d m = case (sum $ Data.List.take m s) == d of
  True -> 1
  False -> 0

birthday :: [Int] -> Int -> Int -> Int -> Int
birthday s d m c = case s of
  [] -> c
  (x:xs) -> birthday xs d m (c + isValidWay s d m)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    sTemp <- getLine

    let s = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip sTemp

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let d = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    let result = birthday s d m 0

    hPrint fptr result

    hFlush fptr
    hClose fptr
