{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Functor ((<&>))

isForbiddenPattern x y z = x == '0' && y == '1' && z == '0'

beautifulBinaryString [] = 0
beautifulBinaryString [x] = 0
beautifulBinaryString [x,y] = 0
beautifulBinaryString (x:y:z:xs)
  | isForbiddenPattern x y z = 1 + beautifulBinaryString (x:y:'1':xs)
  | otherwise = beautifulBinaryString (y:z:xs)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- getLine <&> (read :: String -> Int) . lstrip . rstrip

    getLine >>= hPutStrLn fptr . show . beautifulBinaryString

    hFlush fptr
    hClose fptr
