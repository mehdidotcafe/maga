{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Char (isSpace)
import Data.List (sort, sortOn)
import Data.Map (fromList)

reduce :: [(Int, Char)] -> (Int, String)
reduce [(v, c)] = (v, [c]) 
reduce ((v, c):xs) = (v, c:separator ++ nc)
    where
        separator = if v /= nv then " " else  ""
        (nv, nc) = reduce xs


encryption s = reduce $ sortOn fst $ zipWith (\ f s -> (f `mod` rowSize, s)) [0 .. ] s'
    where
        rowSize = ceiling $ sqrt $ fromIntegral $ length s'
        s' = filter (not . isSpace) s

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let (_, result) = encryption s

    print result

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr

