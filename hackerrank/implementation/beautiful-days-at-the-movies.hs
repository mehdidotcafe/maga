{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.List (words, map)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

reverseInt :: Int -> Int
reverseInt 0 = 0
reverseInt x = (x `mod` 10) * 10 ^ p + reverseInt (x `div` 10)
  where p = (floor . logBase 10) $ fromIntegral x

beautifulDays :: [Int] -> Int -> Int
beautifulDays [] k = 0
beautifulDays (x:xs) k
  | abs (x - reverseInt x) `mod` k == 0 = 1 + nextBeautifulDays
  | otherwise = nextBeautifulDays
  where
    nextBeautifulDays = beautifulDays xs k

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let i = read (firstMultipleInput !! 0) :: Int

    let j = read (firstMultipleInput !! 1) :: Int

    let k = read (firstMultipleInput !! 2) :: Int

    let result = beautifulDays [i..j] k

    print result

    hPutStrLn fptr $ show $ result

    hFlush fptr
    hClose fptr
