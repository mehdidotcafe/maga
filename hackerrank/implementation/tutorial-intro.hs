{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (elemIndex, map, words)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Debug.Trace
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import System.IO.Unsafe

introTutorial = elemIndex

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    vTemp <- getLine
    let v = read $ lstrip $ rstrip vTemp :: Int

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ rstrip arrTemp

    let result = case introTutorial v arr of
          Nothing -> -1
          Just v -> v

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
