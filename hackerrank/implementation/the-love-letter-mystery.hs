  {-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Array ( listArray, (!) )
import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPrint, openFile, IOMode(WriteMode) )

theLoveLetterMystery s = theLoveLetterMystery' (sLength - 1) 0 sAsArray
  where
    sAsArray = listArray (0, sLength) s
    sLength = length s
    theLoveLetterMystery' q i s
      | i < q = (abs ((fromEnum (s!i)) - (fromEnum (s!q)))) + theLoveLetterMystery' (q - 1) (i + 1) s
      | otherwise = 0

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \ q_itr -> getLine >>= hPrint fptr . theLoveLetterMystery

    hFlush fptr
    hClose fptr
