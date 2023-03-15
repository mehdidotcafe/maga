{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Map (empty, insertWith, filter, size)
import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Prelude hiding (filter, length)

isValid [] = True
isValid [x] = False
isValid b = (head b == '_' || head b == head (tail b)) && isValid' b
  where
    isValid' [] = False
    isValid' [x] = False
    isValid' [y, z] = y == z
    isValid' (x:y:z:xs)
      | y == '_' = isValid' (y:z:xs)
      | otherwise = (y == z || y == x) && isValid' (y:z:xs)

happyLadybugs b
  | isValid b = "YES"
  | otherwise = case happyLadybugs' b empty 0 of
    (map, spaceCount) | let hasLbAlone = size (filter (== 1) map) /= 0 in hasLbAlone || (spaceCount == 0 && size map > 1 ) -> "NO"
                      | otherwise -> "YES"
  where
    happyLadybugs' [] map spaceCount = (map, spaceCount)
    happyLadybugs' (x:xs) map spaceCount = happyLadybugs' xs nextMap nextSpaceCount
      where
        (nextMap, nextSpaceCount) = if x == '_' then (map, spaceCount + 1) else (insertWith (+) x 1 map, spaceCount)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    gTemp <- getLine
    let g = read $ lstrip $ rstrip gTemp :: Int

    forM_ [1..g] $ \g_itr -> do
        nTemp <- getLine
        let n = read $ lstrip $ rstrip nTemp :: Int

        b <- getLine

        let result = happyLadybugs b

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
