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

--
-- Complete the 'kangaroo' function below.
--
-- The function is expected to return a STRING.
-- The function accepts following parameters:
--  1. INTEGER x1
--  2. INTEGER v1
--  3. INTEGER x2
--  4. INTEGER v2
--

kangaroo :: Int -> Int -> Int -> Int -> String
kangaroo x1 v1 x2 v2 =  if
    | x1 == x2 -> "YES"
    | x1 > x2 && v1 >= v2 -> "NO"
    | x2 > x1 && v2 >= v1 -> "NO"
    | otherwise -> kangaroo (x1 + v1) v1 (x2 + v2) v2
    
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let x1 = read (firstMultipleInput !! 0) :: Int

    let v1 = read (firstMultipleInput !! 1) :: Int

    let x2 = read (firstMultipleInput !! 2) :: Int

    let v2 = read (firstMultipleInput !! 3) :: Int

    let result = kangaroo x1 v1 x2 v2

    print result

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
