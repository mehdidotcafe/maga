module Main where

import Control.Monad
import Data.List (intercalate, map, words)
import Data.Text (unpack, pack, stripStart, stripEnd)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

getIdx i (x:xs)
  | x == i = 1
  | otherwise = 1 + getIdx i xs

buildArray a = map ((`getIdx` a) . (`getIdx` a)) [1..length a]

permutationEquation = buildArray

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    pTemp <- getLine

    let p = map (read :: String -> Int) . words $ rstrip pTemp

    let result = permutationEquation p

    hPutStrLn fptr $ intercalate "\n" $ map show result

    hFlush fptr
    hClose fptr
