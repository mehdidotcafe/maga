{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Char ( digitToInt )
import Data.List ( map, intercalate, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

acmTeam [x] [] = (0, 0)
acmTeam (x:xs) [] = acmTeam xs (tail xs)
acmTeam (x:xs) (y:ys)
  | maxTopic > nextMaxTopic = (maxTopic, 1)
  | maxTopic == nextMaxTopic = (maxTopic, 1 + numberTeams)
  | otherwise = (nextMaxTopic, numberTeams)
  where
    maxTopic = length $ filter (>= 1) $ zipWith (+) x y
    (nextMaxTopic, numberTeams) = acmTeam (x:xs) ys

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (map digitToInt line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    topic <- readMultipleLinesAsStringArray n

    let (max, topicCount) = acmTeam topic $ tail topic

    print $ max
    print $ topicCount

    hPutStrLn fptr $ show max
    hPutStrLn fptr $ show topicCount

    hFlush fptr
    hClose fptr
