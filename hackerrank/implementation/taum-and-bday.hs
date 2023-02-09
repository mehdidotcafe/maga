{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.List ( words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

taumBday b w bc wc z = bs + ws
  where
    bs = min (b * bc) (b * (wc + z))
    ws = min (w * wc) (w * (bc + z))

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

        let b = read (firstMultipleInput !! 0) :: Int

        let w = read (firstMultipleInput !! 1) :: Int

        secondMultipleInputTemp <- getLine
        let secondMultipleInput = words $ rstrip secondMultipleInputTemp

        let bc = read (secondMultipleInput !! 0) :: Int

        let wc = read (secondMultipleInput !! 1) :: Int

        let z = read (secondMultipleInput !! 2) :: Int

        let result = taumBday b w bc wc z

        print result

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
