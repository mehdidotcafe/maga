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
import qualified Data.Map.Strict as Map

birthdayCakeCandles :: [Int] -> Map.Map Int Int -> Int -> Int
birthdayCakeCandles candles sums maxValue = case candles of
  [] -> maxValue
  (x:xs) -> case Map.lookup x sums of
    Nothing -> birthdayCakeCandles xs (Map.insert x 1 sums) maxValue
    Just y -> birthdayCakeCandles xs (Map.insert x (y + 1) sums) (max (y + 1) maxValue) 

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    candlesCountTemp <- getLine
    let candlesCount = read $ lstrip $ rstrip candlesCountTemp :: Int

    candlesTemp <- getLine

    let candles = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip candlesTemp

    let result = birthdayCakeCandles candles Map.empty minBound

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
