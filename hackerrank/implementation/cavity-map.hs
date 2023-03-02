{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List ( intercalate )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

stringifyMap n i [] = ""
stringifyMap n i (x:xs) = x ++ sep ++ stringifyMap n (i + 1) xs
    where
        sep
            | i `mod` n == n - 1 && i /= (n * n) - 1 = "\n"
            | otherwise = ""

cmpGridValue (_, x1) (_, x2) = x1 < x2

cavityMap n grid = map (\ (i, x) -> if isCavity i x then "X" else show x) grid
    where
        isCavity i x = lv && bv && tv && rv
            where
            lv = i - 1 >= 0 && ((i - 1) `div` n) == (i `div` n) && cmpGridValue (grid!!(i - 1)) (grid!!i)
            bv = i + n < (n * n) && cmpGridValue (grid!!(i + n)) (grid!!i)
            tv = i - n >= 0 && cmpGridValue (grid!!(i - n)) (grid!!i)
            rv = i + 1 < (n * n) && ((i + 1) `div` n) == (i `div` n) && cmpGridValue (grid!!(i + 1)) (grid!!i)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsIntList :: Int -> IO [Int]
readMultipleLinesAsIntList 0 = return []
readMultipleLinesAsIntList n = do
    line <- getLine
    rest <- readMultipleLinesAsIntList(n - 1)
    return (map digitToInt line ++ rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    grid <- readMultipleLinesAsIntList n

    let result = stringifyMap n 0 $ cavityMap n $ zip [0..] grid

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr