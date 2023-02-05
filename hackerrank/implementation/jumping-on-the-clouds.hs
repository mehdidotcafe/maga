{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

jumpingOnClouds [x,xs] = 1
jumpingOnClouds [x] = 0
jumpingOnClouds (x:xs:xxs)
  | head xxs == 1 = 1 + jumpingOnClouds (xs:xxs)
  | otherwise = 1 + jumpingOnClouds xxs

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    cTemp <- getLine

    let c = map (read :: String -> Int) . words $ rstrip cTemp

    let result = jumpingOnClouds c

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
