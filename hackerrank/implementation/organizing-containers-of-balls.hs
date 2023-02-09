{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.List ( map, words , sort )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

addMatrixVertically [x] = x
addMatrixVertically (x:xs) = zipWith (+) x $ addMatrixVertically xs

addMatrixHorizontally = map sum

organizingContainers containers
    | horizontalSums == verticalSums = "Possible"
    | otherwise = "Impossible"
    where
        horizontalSums = sort $ addMatrixHorizontally containers
        verticalSums = sort $ addMatrixVertically containers

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \q_itr -> do
        nTemp <- getLine
        let n = read $ lstrip $ rstrip nTemp :: Int

        containerTemp <- readMultipleLinesAsStringArray n
        let container = map (map (read :: String -> Int) . words . rstrip) containerTemp

        let result = organizingContainers container

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
