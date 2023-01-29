{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (map, words)
import Data.Text (pack, unpack, stripStart, stripEnd)
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

magicSquareCombinations :: [[[Int]]]
magicSquareCombinations = [
  [[8,1,6], [3,5,7], [4,9,2]],
  [[6,1,8], [7,5,3], [2,9,4]],
  [[4,9,2], [3,5,7], [8,1,6]],
  [[2,9,4], [7,5,3], [6,1,8]],

  [[8,3,4], [1,5,9], [6,7,2]],
  [[4,3,8], [9,5,1], [2,7,6]],
  [[6,7,2], [1,5,9], [8,3,4]],
  [[2,7,6], [9,5,1], [4,3,8]]
  ]

formingMagicSquare s = minimum . map (sum . map (sum . map(abs . uncurry (-)) . uncurry zip )) $ map (zip s) magicSquareCombinations
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

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

    sTemp <- readMultipleLinesAsStringArray 3
    let s = map (\x -> map (read :: String -> Int) . words $ rstrip x) sTemp

    let result = formingMagicSquare s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
