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

--
-- Complete the 'staircase' function below.
--
-- The function accepts INTEGER n as parameter.
--

staircase :: Int -> Int -> String -> String
staircase n idx output = if idx < n then
    staircase n (idx + 1) (output ++ Data.List.take spaces (repeat ' ') ++ Data.List.take hashes (repeat '#') ++ "\n")
    else output
  where
    spaces = n - idx - 1
    hashes = idx + 1

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    putStrLn $ staircase n 0 ""
