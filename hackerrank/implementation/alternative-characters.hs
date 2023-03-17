{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPrint, openFile, IOMode(WriteMode) )

alternatingCharacters [] = 0
alternatingCharacters [x] = 0
alternatingCharacters (x:y:xs)
  | x == y = 1 + n
  | otherwise = n
  where
    n = alternatingCharacters (y:xs)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \ q_itr -> getLine >>= hPrint fptr . alternatingCharacters

    hFlush fptr
    hClose fptr
