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
import Data.Maybe

update :: (Int -> Bool) -> (Int, Maybe Int) -> Int -> (Int, Maybe Int)
update cmp (highestCount, highestValue) value = case highestValue of
  Nothing -> (highestCount, Just value)
  Just highest -> if cmp highest then (highestCount + 1, Just value) else (highestCount, highestValue)


breakingRecords :: [Int] -> (Int, Maybe Int) -> (Int, Maybe Int) -> (Int, Int)
breakingRecords scores (lowestCount, lowestValue) (highestCount, highestValue) = case scores of
  [] -> (lowestCount, highestCount)
  (x: xs) -> breakingRecords xs (update (< x) (lowestCount, lowestValue) x) (update (> x) (highestCount, highestValue) x)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    scoresTemp <- getLine

    let scores = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip scoresTemp

    let result = breakingRecords scores (0, Nothing) (0, Nothing)

    hPutStr fptr $ show $ snd result
    hPutStr fptr " "
    hPutStrLn fptr $ show $ fst result

    hFlush fptr
    hClose fptr
