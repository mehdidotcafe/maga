{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.List ( words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

saveThePrisoner n m s = ((m + s - 2) `mod` n) + 1
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
        let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

        let n = read (firstMultipleInput !! 0) :: Int

        let m = read (firstMultipleInput !! 1) :: Int

        let s = read (firstMultipleInput !! 2) :: Int

        let result = saveThePrisoner n m s

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
