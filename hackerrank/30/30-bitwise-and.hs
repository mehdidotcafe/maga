{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.List ( words )
import Data.Text ( unpack, pack, stripEnd, stripStart )
import System.Environment ( getEnv )
import System.IO
    ( IOMode(WriteMode), hClose, hFlush, hPutStrLn, openFile )
import Data.Bits ((.&.))

toto i j c k
  | nv > v && nv < k = nv
  | v < k = v
  | otherwise = 0
  where
    v = (.&.) i j
    nv = bitwiseAnd i (j + 1) c k

bitwiseAnd i j c k
  | i > c = 0
  | j > c = bitwiseAnd (i + 1) (i + 2) c k
  | otherwise = toto i j c k

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

        let count = read (firstMultipleInput !! 0) :: Int

        let lim = read (firstMultipleInput !! 1) :: Int

        let res = bitwiseAnd 1 2 count lim

        hPutStrLn fptr $ show res

    hFlush fptr
    hClose fptr
