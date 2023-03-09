{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Char (toLower)
import Data.Set (fromList, size, member, delete, null)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Prelude hiding (null)

pangrams [] ls = if null ls then "pangram" else "not pangram"
pangrams (x:xs) ls
  | member x' ls = pangrams xs $ delete x' ls
  | otherwise = pangrams xs ls
  where
    x' = toLower x

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = pangrams s $ fromList ['a' .. 'z']

    print result

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
