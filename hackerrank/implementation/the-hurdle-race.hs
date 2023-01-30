{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List (maximum, map, words)
import Data.Text (unpack, pack, stripStart, stripEnd)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

hurdleRace k = max 0 . subtract k . maximum 

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    heightTemp <- getLine

    let height = map (read :: String -> Int) . words $ rstrip heightTemp

    let result = hurdleRace k height

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
