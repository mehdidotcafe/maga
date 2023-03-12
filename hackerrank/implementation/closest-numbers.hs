{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Map (empty, insertWith, lookupMin)
import Data.List ( map, words, intercalate, sort )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Maybe (isNothing)


calcDifference m [x] = m
calcDifference m (x:y:xs) = calcDifference nm (y:xs)
  where
    nm = insertWith (flip (++)) d [x, y] m
    d = y - x

closestNumbers = maybe [] snd . lookupMin . calcDifference empty . sort
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

    let result = closestNumbers arr

    hPutStrLn fptr $ unwords $ map show result

    hFlush fptr
    hClose fptr
