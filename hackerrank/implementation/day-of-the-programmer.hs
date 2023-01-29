{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Debug.Trace
import System.Environment
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import System.IO.Unsafe

getJulianDay :: Integral a => a -> String
getJulianDay year 
  | year `mod` 4 == 0 = "12"
  | otherwise = "13"

getGregorianDay year
  | (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0 = "12"
  | otherwise = "13"

dayOfProgrammer year
    | year < 1918 = getJulianDay year ++ monthYear
    | year == 1918 = "26" ++ monthYear
    | otherwise = getGregorianDay year ++ monthYear
  where monthYear = ".09." ++ show year

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    yearTemp <- getLine
    let year = read $ lstrip $ rstrip yearTemp :: Int

    let result = dayOfProgrammer year

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
