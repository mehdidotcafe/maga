{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (length, map ,words)
import Data.Text ( unpack, pack, stripEnd, stripStart )
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

pickingNumbers (x:xs) = max (max lowerSerieLength upperSerieLength) $ pickingNumbers xs
  where
      lowerSerieLength = length [ y | y <- xs, y == x + 1 || y == x] + 1
      upperSerieLength = length [ y | y <- xs, y == x - 1 || y == x] + 1

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    aTemp <- getLine

    let a = map (read :: String -> Int) . words $ rstrip aTemp

    let result = pickingNumbers a

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
