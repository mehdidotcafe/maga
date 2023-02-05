{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Prelude hiding (foldr)
import Data.List (take, replicate, sort, group, foldr)
import Data.Text (unpack, stripStart, stripEnd, pack)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Map (fromListWith)


repeatedString n arr = foldr (\ i acc -> acc + comp i) 0 [ (x, f i) | (x, i) <- zip arr [0..]]
  where
    comp ('a', v) = v
    comp (_, v) = 0

    f i = n `div` l + if i < (n `mod` l) then 1 else 0
    l = length arr

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    let result = repeatedString n s

    hPutStrLn fptr $ show result  

    hFlush fptr
    hClose fptr
