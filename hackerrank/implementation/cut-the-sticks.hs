{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List ( map, words, intercalate, length, minimum )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

cutSticks [] s = [] 
cutSticks (x:xs) s
  | x - s > 0 = (x - s) : cutSticks xs s
  | otherwise = cutSticks xs s

cutTheSticks [] = []
cutTheSticks arr = length arr : cutTheSticks nextSticks
  where
    nextSticks = cutSticks arr smallestSick
    smallestSick = minimum arr

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ rstrip arrTemp

    let result = cutTheSticks arr

    hPutStrLn fptr $ intercalate "\n" $ map show result

    hFlush fptr
    hClose fptr
