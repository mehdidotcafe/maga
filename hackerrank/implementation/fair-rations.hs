{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

fairRations :: [Int] -> Int
fairRations [x]
  | odd x = -1
  | otherwise = 0
fairRations (x:y:xs) = case fairRations (ny:xs) of
  -1 -> -1
  v -> cv + v
  where
    (cv, ny) | even x = (0, y) | otherwise = (2, y + 1)
lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    bTemp <- getLine

    let b = map (read :: String -> Int) . words $ rstrip bTemp

    let result = (case fairRations b of -1 -> "NO"
                                        v -> show v)

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
