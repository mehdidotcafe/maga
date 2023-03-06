{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Array (Array, array, (!))
import Data.Char (digitToInt)
import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Functor ((<&>))
import Debug.Trace (trace)

countFaces a h w i = lfc + rfc + tfc + bfc
  where
    lfc | i - 1 >= 0 && div (i - 1) w == div i w = max 0 (a!i - a!(i-1)) | otherwise = a!i
    rfc | i + 1 < h * w && div (i + 1) w == div i w = max 0 (a!i - a!(i+1)) | otherwise = a!i
    tfc | i - w >= 0 = max 0 (a!i - a!(i-w)) | otherwise = a!i
    bfc | i + w < h * w = max 0 (a!i - a!(i+w)) | otherwise = a!i

surfaceArea a h w = surfaceArea' a h w (h * w - 1)
  where
    surfaceArea' a h w (-1) = 0
    surfaceArea' a h w i = 2 + faces + surfaceArea' a h w (i - 1)
      where
          faces = countFaces a h w i

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

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let h = read (firstMultipleInput !! 0) :: Int

    let w = read (firstMultipleInput !! 1) :: Int

    aTemp <- readMultipleLinesAsStringArray h

    let a = array (0, h * w - 1) $ zip [0..] (concatMap (map (read :: String -> Int) . words . rstrip) aTemp)

    let result = surfaceArea a h w

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
