{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, length, words, sort )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPrint, openFile, IOMode(WriteMode) )
import Data.Functor ((<&>))

findMedian n arr = sarr !! hn
  where
    hn = n `div` 2
    sarr = sort arr

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- getLine <&> (read :: String -> Int) . lstrip . rstrip
    arr <- getLine <&> map (read :: String -> Int) . words . rstrip

    hPrint fptr (findMedian n arr)

    hFlush fptr
    hClose fptr
