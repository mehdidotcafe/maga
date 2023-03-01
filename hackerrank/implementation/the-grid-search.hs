{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Array (Array, array, (!))
import Control.Monad ( forM_ )
import Data.List ( words, map )
import Data.Text ( unpack, pack, stripEnd, stripStart )
import Data.Char (digitToInt)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Functor ((<&>))

data Search = Yes | No deriving (Eq, Ord, Show, Enum)

gridSearch g p (gc, gr, gn) (pc, pr, pn) gi
    | gi >= gn = "NO"
    | otherwise = case gridSearch' g p (gc, gr, gn) (pc, pr, pn) gi 0 of
        Yes -> "YES"
        No -> gridSearch g p (gc, gr, gn) (pc, pr, pn) (gi + 1)
    where
        gridSearch' g p (gc, gr, gn) (pc, pr, pn) gi pi
            | pi >= pn = Yes
            | gi >= gn = No
            | (ngi - gi) == 1 && ngi `div` gc /= gi `div` gc = No
            | g!gi == p!pi = gridSearch' g p (gc, gr, gn) (pc, pr, pn) ngi (pi + 1)
            | otherwise = No
            where
                ngi
                    | pi `mod` pc == pc - 1 = gi + gc - pc + 1
                    | otherwise = gi + 1

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsIntArray :: Int -> Int -> IO (Array Int Int)
readMultipleLinesAsIntArray n c = readMultipleLinesAsStringList n <&> (array (0, n * c - 1) . zip [0..])
    where
        readMultipleLinesAsStringList :: Int -> IO [Int]
        readMultipleLinesAsStringList 0 = return []
        readMultipleLinesAsStringList n = do
            line <- getLine
            rest <- readMultipleLinesAsStringList(n - 1)
            return (map digitToInt line ++ rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = words $ rstrip firstMultipleInputTemp

        let r = read (firstMultipleInput !! 0) :: Int

        let c = read (firstMultipleInput !! 1) :: Int

        g <- readMultipleLinesAsIntArray r c

        secondMultipleInputTemp <- getLine
        let secondMultipleInput = words $ rstrip secondMultipleInputTemp

        let r2 = read (secondMultipleInput !! 0) :: Int

        let c2 = read (secondMultipleInput !! 1) :: Int

        p <- readMultipleLinesAsIntArray r2 c2

        let result = gridSearch g p (c, r, c * r) (c2, r2, c2 * r2) 0

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
