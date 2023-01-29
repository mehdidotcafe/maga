{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (null)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

countingValleys :: String -> Int -> Int
countingValleys arr c = case arr of
  [] -> 0
  (x:xs) -> case x of
    'D' -> countingValleys xs (c - 1)
    'U' -> (if (c + 1) == 0 then 1 else 0) + countingValleys xs (c + 1)    

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    stepsTemp <- getLine
    let steps = read $ lstrip $ rstrip stepsTemp :: Int

    path <- getLine

    let result = countingValleys path 0

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
