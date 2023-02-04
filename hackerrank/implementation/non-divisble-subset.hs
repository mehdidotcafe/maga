module Main where

import Data.List ( map, words, length, all )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

-- https://www.geeksforgeeks.org/subset-no-pair-sum-divisible-k/

buildRemindersArray k [] r = r
buildRemindersArray k (x:xs) r = buildRemindersArray k xs r'
  where
      r' = take m r ++ ( (r!!m) + 1) : drop (m + 1) r
      m = x `mod` k


addReminders r k i
  | i <= k `div` 2 = n + addReminders r k (i + 1)
  | otherwise = 0
  where
    n
      | i == 0 = min (r!!i) 1
      | even k && i == k `div` 2 = min (r!!i) 1
      | otherwise = max (r!!i) (r!!(k - i))

nonDivisibleSubset k s = addReminders r k 0
  where
    r = buildRemindersArray k s (replicate k 0)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    sTemp <- getLine

    let s = map (read :: String -> Int) . words $ rstrip sTemp

    hPutStrLn fptr $ show $ nonDivisibleSubset k s

    hFlush fptr
    hClose fptr
