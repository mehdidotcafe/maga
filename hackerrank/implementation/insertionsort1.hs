{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List ( map, words )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Debug.Trace (trace)


insertionSort1 a = case insertionSort1' a a of
    (_, v, _, b)
      | v == minBound -> b
      | otherwise -> b ++ [v : tail (last b)]
  where
    insertionSort1' [x, y] a = ([x, x], y, init $ init a, [init a ++ [x]])
    insertionSort1' (x:xs) a = case insertionSort1' xs a of
      (txs, toSort, a, aStr)
        | toSort == minBound -> (x : txs, toSort, init a, aStr)
        | toSort > x -> (x:(toSort : tail txs), minBound, init a, aStr ++ [init a ++ (x:(toSort : tail txs))])
        | otherwise -> (x:(x : tail txs), toSort, init a, aStr ++ [init a ++ (x:(x : tail txs))])


stringify [] = ""
stringify (x:xs) = unwords (map show x) ++ "\n" ++ stringify xs

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ rstrip arrTemp

    putStr $ stringify $ insertionSort1 arr
