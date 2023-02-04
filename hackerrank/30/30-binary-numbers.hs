module Main where

import Data.Bits ( Bits(testBit) )
import Data.Text ( pack, stripEnd, stripStart, unpack )

getFalseBitSerie n (-1) = 0
getFalseBitSerie n b 
  | not (testBit n b) = 1 + getFalseBitSerie n (b - 1)
  | otherwise = 0

getTrueBitSerie n (-1) = 0
getTrueBitSerie n b
  | testBit n b = 1 + getTrueBitSerie n (b - 1)
  | otherwise = 0

consecutiveBits n (-1) = 0
consecutiveBits n b = max tb (consecutiveBits n (b - tb - fb))
  where
    tb = getTrueBitSerie n b
    fb = getFalseBitSerie n (b - tb)



lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    print $ consecutiveBits n 32