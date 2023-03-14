{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (forM_)
import Data.List (intercalate, words)
import Data.Map (foldr, fromList, insert, toAscList, (!))
import Data.Maybe (isNothing)
import Data.Text (pack, stripEnd, stripStart, unpack)
import Debug.Trace (trace)
import System.Environment (getEnv)
import System.IO
  ( IOMode (WriteMode),
    hClose,
    hFlush,
    hPutStrLn,
    openFile,
  )
import Prelude hiding (foldr)

swapInMap map (k1, v1) (k2, v2) = insert k1 v1 $ insert k2 v2 map

absolutePermutation n i k map
  | i > n = Just map
  | (i + k) <= n && map ! i == map ! (i + k) - k = absolutePermutation n (i + 1) k $ swapInMap map (i, map ! (i + k)) (map ! (i + k), i)
  | abs (i - map ! i) /= k = Nothing
  | otherwise = absolutePermutation n (i + 1) k map

lstrip = unpack . stripStart . pack

rstrip :: String -> String
rstrip = unpack . stripEnd . pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  tTemp <- getLine
  let t = read $ lstrip $ rstrip tTemp :: Int

  forM_ [1 .. t] $ \t_itr -> do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    let result = case () of _
                              | k == 0 -> unwords $ map show [1 .. n]
                              | odd n -> "-1"
                              | otherwise -> case absolutePermutation n 1 k (fromList [(i, i) | i <- [1 .. n]]) of
                                  Nothing -> "-1"
                                  Just v -> unwords $ map (show . snd) $ toAscList v

    hPutStrLn fptr result

  hFlush fptr
  hClose fptr
