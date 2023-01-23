{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances, MultiWayIf #-}

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
-- Complete the 'timeConversion' function below.
--
-- The function is expected to return a STRING.
-- The function accepts STRING s as parameter.
--

getMinAndSec :: String -> String
getMinAndSec s = Data.List.drop 2 (Data.List.take 8 s)

timeConversion s = if
  | s!!8 == 'A' && s!!0 == '1' && s!!1 == '2' -> "00" ++ getMinAndSec s
  | s!!8 == 'A' -> Data.List.take 8 s
  | s!!8 == 'P' && s!!0 == '1' && s!!1 == '2' -> Data.List.take 8 s
  | s!!8 == 'P' -> show (12 + read (Data.List.take 2 s)) ++ getMinAndSec s

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = timeConversion s

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
