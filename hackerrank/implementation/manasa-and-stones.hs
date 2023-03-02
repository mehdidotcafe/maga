{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.IntSet (empty, insert, size, IntSet, foldr, fromList, map, toList)
import Control.Monad ( forM_ )
import Data.List ( intercalate, nub, sort )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Prelude hiding (foldr, map)

stones 1 a b m = m
stones n a b m = stones (n - 1) a b $ foldr (\x acc -> insert (x + a) $ insert (x + b) acc) empty m
lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

stringify = foldr (\x acc -> show x ++ " " ++ acc) ""

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        nTemp <- getLine
        let n = read $ lstrip $ rstrip nTemp :: Int

        aTemp <- getLine
        let a = read $ lstrip $ rstrip aTemp :: Int

        bTemp <- getLine
        let b = read $ lstrip $ rstrip bTemp :: Int

        let result = stones n a b $ fromList [0]

        hPutStrLn fptr $ stringify result

    hFlush fptr
    hClose fptr
