{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( intercalate, sortBy )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )


cmp s1 s2
  | length s1 > length s2 = GT
  | length s2 > length s1 = LT
  | otherwise = cmpFirstDigit s1 s2
  where
      cmpFirstDigit [] [] = EQ
      cmpFirstDigit (x:xs) (y:ys)
        | x > y = GT
        | y > x = LT
        | otherwise = cmpFirstDigit xs ys

bigSorting = sortBy cmp

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    unsorted <- readMultipleLinesAsStringArray n

    let result = bigSorting unsorted

    hPutStrLn fptr $ intercalate "\n" result

    hFlush fptr
    hClose fptr
