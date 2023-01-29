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

bonAppetit bill k b 
  | sumAnna == b = "Bon Appetit"
  | otherwise = show (b - sumAnna)
  where sumAnna = (sum bill - bill!!k) `div` 2

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    billTemp <- getLine

    let bill = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip billTemp

    bTemp <- getLine
    let b = read $ lstrip $ rstrip bTemp :: Int

    putStrLn $ bonAppetit bill k b
