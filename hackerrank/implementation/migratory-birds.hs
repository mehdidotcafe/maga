{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (sort, map, words, group, foldr)
import Data.Text (unpack, stripStart, stripEnd, pack)
import Debug.Trace
import System.Environment
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import System.IO.Unsafe
import Control.Arrow ((&&&))

comp (length, head) (accLength, accHead)
  | length > accLength || (length == accLength && head < accHead) = (length, head)
  | otherwise = (accLength, accHead)

migratoryBirds = snd . foldr comp (0, 0) . map (length &&& head) . group . sort
lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arrCountTemp <- getLine
    let arrCount = read $ lstrip $ rstrip arrCountTemp :: Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ rstrip arrTemp

    let result = migratoryBirds arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
