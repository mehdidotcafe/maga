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
import Numeric
import Distribution.Utils.ShortText (decodeStringUtf8)

--
-- Complete the 'plusMinus' function below.
--
-- The function accepts INTEGER_ARRAY arr as parameter.
--

decimals = 6

plusMinus :: [Int] -> [Int]
plusMinus arr = Data.List.foldr (\ x [positive, negative, zero] -> case compare x 0 of 
    LT -> [positive, negative + 1, zero]
    EQ -> [positive, negative, zero + 1]
    GT -> [positive + 1, negative, zero]
  ) [0, 0, 0] arr
  


convertToFloat :: [Int] -> [Float]
convertToFloat [positive, negative, zero]  = Data.List.map (\ x -> fromIntegral x / fromIntegral length) [positive, negative, zero]
  where length = positive + negative + zero

printFloats :: Int -> [Float] -> IO()
printFloats d [positive, negative, zero] = do
  putStrLn $ showFFloat (Just d) positive ""
  putStrLn $ showFFloat (Just d) negative ""
  putStrLn $ showFFloat (Just d) zero ""

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    printFloats decimals . convertToFloat $ plusMinus arr
