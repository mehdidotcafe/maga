{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Char (isUpper)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

camelcase [y] = 1
camelcase (x:y:xs)
  | isUpper y = 1 + n
  | otherwise = n
  where
    n = camelcase (y:xs)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = camelcase s

    print result

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
