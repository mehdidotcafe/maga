{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

chocolateFeast n c m = b + wrapperToChocolate b
  where
    b = n `div` c
    wrapperToChocolate w
      | w >= m = bw + wrapperToChocolate (w `mod` m + bw)
      | otherwise = 0
      where bw = w `div` m

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = words $ rstrip firstMultipleInputTemp

        let n = read (firstMultipleInput !! 0) :: Int

        let c = read (firstMultipleInput !! 1) :: Int

        let m = read (firstMultipleInput !! 2) :: Int

        hPutStrLn fptr $ show $ chocolateFeast n c m

    hFlush fptr
    hClose fptr
