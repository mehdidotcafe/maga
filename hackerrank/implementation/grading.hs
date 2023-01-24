{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (foldr, intercalate, map)
import Data.Text (pack, stripEnd, stripStart, unpack)
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

eventuallyRoundUp :: Int -> Int
eventuallyRoundUp x = if modRes < 3 || x < 38 then x else x + 5 - modRes
  where
    modRes = x `mod` 5

gradingStudents :: [Int] -> [Int]
gradingStudents grades = Data.List.map eventuallyRoundUp grades

-- gradingStudents grades = foldr (\ x -> if x mod 5 < 3 then (:) x else (:) (x + x mod 5)) [] grades

lstrip :: String -> String
lstrip = unpack . stripStart . pack

rstrip = unpack . stripEnd . pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
  line <- getLine
  rest <- readMultipleLinesAsStringArray (n - 1)
  return (line : rest)

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  gradesCountTemp <- getLine
  let gradesCount = read $ lstrip $ rstrip gradesCountTemp :: Int

  gradesTemp <- readMultipleLinesAsStringArray gradesCount
  let grades = Data.List.map (read :: String -> Int) gradesTemp

  let result = gradingStudents grades

  hPutStrLn fptr $ Data.List.intercalate "\n" $ Data.List.map show result

  hFlush fptr
  hClose fptr
