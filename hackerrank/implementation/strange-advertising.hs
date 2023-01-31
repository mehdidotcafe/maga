{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

advertisedPeopleAmount = 5
sharedToPeopleAmount = 3

viralAdvertising p 0 = 0
viralAdvertising p n = s + viralAdvertising (s * sharedToPeopleAmount) (n - 1)
  where
    s = floor (fromIntegral p / 2)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    let result = viralAdvertising advertisedPeopleAmount n

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
