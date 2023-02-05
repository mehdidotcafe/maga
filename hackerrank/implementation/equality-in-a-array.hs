{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Arrow ((&&&))
import Data.List ( map, words, sort, group, maximum )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

equalizeArray n arr = n - maximum s
  where
    s = map length $ group $ sort arr

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

    let result = equalizeArray n arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
