{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List (words)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPrint, openFile, IOMode(WriteMode) )


howManyGames p d m s
  | p <= s = 1 + howManyGames np d m (s - p)
  | otherwise = 0
  where
    np = max (p - d) m

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let p = read (firstMultipleInput !! 0) :: Int

    let d = read (firstMultipleInput !! 1) :: Int

    let m = read (firstMultipleInput !! 2) :: Int

    let s = read (firstMultipleInput !! 3) :: Int

    hPrint fptr (howManyGames p d m s)

    hFlush fptr
    hClose fptr
