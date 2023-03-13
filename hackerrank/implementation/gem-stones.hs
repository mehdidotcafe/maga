{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Set (fromList, intersection, size)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

gemstones (x:xs) = foldr intersection x xs

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arr <- readMultipleLinesAsStringArray n

    let result = gemstones $ map fromList arr

    hPutStrLn fptr $ show $ size result

    hFlush fptr
    hClose fptr
