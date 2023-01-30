{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Array
import Data.List (map, words, intercalate)
import Data.Text (unpack, stripStart, stripEnd, pack)

import System.Environment
import System.IO
import System.IO.Unsafe


climbingLeaderboard :: [Int] -> [Int] -> Int -> [Int]
-- all remaining player's score are lower than leaderboard lowest score
climbingLeaderboard [] p idx = replicate (length p) idx
-- we checked every player's score
climbingLeaderboard r [] idx = []
climbingLeaderboard (r:xr) (p:xp) idx
    | not (null xr) && r == head xr = climbingLeaderboard xr (p:xp) idx
    | p >= r = idx : climbingLeaderboard (r:xr) xp idx
    | otherwise = climbingLeaderboard xr (p:xp) (idx + 1)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    rankedCountTemp <- getLine
    let rankedCount = read $ lstrip $ rstrip rankedCountTemp :: Int

    rankedTemp <- getLine

    let ranked = map (read :: String -> Int) . words $ rstrip rankedTemp

    playerCountTemp <- getLine
    let playerCount = read $ lstrip $ rstrip playerCountTemp :: Int

    playerTemp <- getLine

    let player = map (read :: String -> Int) . words $ rstrip playerTemp

    let result = climbingLeaderboard ranked (reverse player) 1

    hPutStrLn fptr $ intercalate "\n" $ map show $ reverse result

    hFlush fptr
    hClose fptr