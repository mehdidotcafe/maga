{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'countApplesAndOranges' function below.
--
-- The function accepts following parameters:
--  1. INTEGER s
--  2. INTEGER t
--  3. INTEGER a
--  4. INTEGER b
--  5. INTEGER_ARRAY apples
--  6. INTEGER_ARRAY oranges
--

isInHouse :: Int -> Int -> Int -> Int -> Bool
isInHouse houseStartX houseEndX treeX appleOffset = appleX >= houseStartX && appleX <= houseEndX
  where
    appleX = treeX + appleOffset

countApplesAndOranges :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> Int -> Int -> (Int, Int)
countApplesAndOranges s t a b apples oranges cApples cOranges = case (apples, oranges) of
    ([], []) -> (cApples, cOranges)
    (apple: appleTail, orange: orangeTail) -> countApplesAndOranges s t a b appleTail orangeTail (if isInHouse s t a apple then cApples + 1 else cApples) (if isInHouse s t b orange then cOranges + 1 else cOranges)
    (apple: appleTail, []) -> countApplesAndOranges s t a b appleTail [] (if isInHouse s t a apple then cApples + 1 else cApples) cOranges
    ([], orange: orangeTail) -> countApplesAndOranges s t a b [] orangeTail cApples (if isInHouse s t b orange then cOranges + 1 else cOranges)

showCounts :: (Int, Int) -> IO()
showCounts (cApples, cOranges) = do
  print cApples
  print cOranges

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let s = read (firstMultipleInput !! 0) :: Int

    let t = read (firstMultipleInput !! 1) :: Int

    secondMultipleInputTemp <- getLine
    let secondMultipleInput = Data.List.words $ rstrip secondMultipleInputTemp

    let a = read (secondMultipleInput !! 0) :: Int

    let b = read (secondMultipleInput !! 1) :: Int

    thirdMultipleInputTemp <- getLine
    let thirdMultipleInput = Data.List.words $ rstrip thirdMultipleInputTemp

    let m = read (thirdMultipleInput !! 0) :: Int

    let n = read (thirdMultipleInput !! 1) :: Int

    applesTemp <- getLine

    let apples = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip applesTemp

    orangesTemp <- getLine

    let oranges = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip orangesTemp

    showCounts $ countApplesAndOranges s t a b apples oranges 0 0
