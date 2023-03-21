{-# LANGUAGE TupleSections #-}

import Data.Map (fromListWith, differenceWith)
import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Data.List ( map )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, openFile, hPrint, IOMode(WriteMode) )

diff a b
  | v <= 0 = Nothing
  | otherwise = Just v
  where
    v = a - b

anagram s
  | odd sLength = -1
  | otherwise = sum $ differenceWith diff s1 s2
  where
    sLength = length s
    toMap s = fromListWith (+) $ map (, 1) s
    s1Sum = sum s1
    s2Sum = sum s2
    s1 = toMap $ take (sLength `div` 2) s
    s2 = toMap $ drop (sLength `div` 2) s

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \ q_itr -> getLine >>= hPrint fptr . anagram

    hFlush fptr
    hClose fptr
