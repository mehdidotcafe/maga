{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Data.List (length, words, map)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Control.Monad

angryProfessor k a = case length [x | x <- a, x <= 0] >= k of
  True -> "NO"
  False -> "YES"

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = words $ rstrip firstMultipleInputTemp

        let n = read (firstMultipleInput !! 0) :: Int

        let k = read (firstMultipleInput !! 1) :: Int

        aTemp <- getLine

        let a = map (read :: String -> Int) . words $ rstrip aTemp

        let result = angryProfessor k a

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
