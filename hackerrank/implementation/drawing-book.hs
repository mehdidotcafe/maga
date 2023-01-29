{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

pageCount :: Int -> Int -> Int
pageCount n p = min m rm
  where
      m = ceiling (fromIntegral (p + 1) / 2) - 1
      rm = floor (fromIntegral  n / 2) - m

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    pTemp <- getLine
    let p = read $ lstrip $ rstrip pTemp :: Int

    let result = pageCount n p

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
