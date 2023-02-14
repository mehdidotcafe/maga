{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Map (empty, insertWith)
import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

defaultValue = -1

getDistances [] i m = m
getDistances (x:xs) i m = getDistances xs (i + 1) nm
  where 
    nm = insertWith (++) x [i] m

comp [f, s] acc
  | acc == defaultValue || acc > d = d
  | otherwise = acc
  where
    d = abs (f - s)
comp _ acc = acc

minimumDistances a = foldr comp defaultValue (getDistances a 0 empty)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    aTemp <- getLine

    let a = map (read :: String -> Int) . words $ rstrip aTemp

    let result = minimumDistances a

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
