{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances, MultiWayIf  #-}

module Main where

import Data.List
-- import Data.List.Split
import Data.Set
import Data.Text
import System.Environment
import System.IO

--
-- Complete the 'simpleArraySum' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY ar as parameter.
--

compareTriplets :: [Int] -> [Int] -> (Int, Int)
compareTriplets a b = Data.List.foldr (\ (aElem, bElem) (aCount, bCount)  -> if
  |  aElem > bElem -> (aCount + 1, bCount)
  |  aElem < bElem -> (aCount, bCount + 1)
  |  otherwise -> (aCount, bCount))
  (0, 0)  $ Data.List.zip a b

lstrip :: String -> String
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"

    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp

    let result = compareTriplets a b

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ [fst result, snd result]

    hFlush fptr
    hClose fptr