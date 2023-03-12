-- Do not copy this code, that's a mess

{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Array (listArray, (!))
import Data.List ( map, words, sort)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Data.Maybe (isNothing)

data Phase = Order | Reverse deriving (Eq, Show)

oneSwapDiff [] [] i = []
oneSwapDiff (x:xs) (y:ys) i
  | x /= y = i : nDiff
  | otherwise = nDiff
  where
    nDiff = oneSwapDiff xs ys (i + 1)

reverseDiff [] p q s i = (q, s, i)
reverseDiff (x:y:xs) p q s i
  | x < y && p == Reverse = (x:q, s, i)
  | x < y && p == Order = reverseDiff (y:xs) p q s (i + 1)
  | x > y && p == Reverse = reverseDiff (y:xs) p (x:q) s (i + 1)
  | x > y && p == Order = reverseDiff (y:xs) Reverse (x:q) i (i + 1)

reverseArraySlice arr q v1 v2 = ra
  where
    ra = take v1 arr ++ q ++ drop (v2 + 1) arr

almostSorted arr = case oneSwapDiff arr sortedArray 1 of
    [v1, v2] -> ("SWAP", v1, v2)
    swapOtherwise -> case reverseDiff arr Order [] 0 0 of
      ([], _, _) -> ("NO", 0, 0)
      (q, v1', v2') -> if reverseArraySlice arr q v1' v2' == sortedArray then ("REVERSE", v1' + 1, v2' + 1) else ("NO", 0, 0)
  where
    sortedArray = sort arr


lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = map (read :: String -> Int) . words $ rstrip arrTemp

    case almostSorted arr of 
      ("NO", _, _) -> putStrLn "no"
      ("SWAP", v1, v2) -> putStrLn ("yes\nswap " ++ show v1 ++ " " ++ show v2)
      ("REVERSE", v1, v2)  -> putStrLn ("yes\nreverse " ++ show v1 ++ " " ++ show v2)