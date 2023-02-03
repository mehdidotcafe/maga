module Main where

import Control.Monad
import Data.List (map, words, filter, length)
import Data.Text (unpack, stripStart, stripEnd, pack)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Map (empty, Map)

isInt x = x == fromInteger (round x)

squares :: Int -> Int -> Int
squares a b = max (floor sb - ceiling sa + 1) 0
    where
        sa = sqrt $ fromIntegral a
        sb = sqrt $ fromIntegral b

lstrip :: String -> String
lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    let m = empty

    forM_ [1..q] $ \q_itr -> do
        firstMultipleInputTemp <- getLine
        let firstMultipleInput = words $ rstrip firstMultipleInputTemp

        let a = read (firstMultipleInput !! 0) :: Int

        let b = read (firstMultipleInput !! 1) :: Int

        let result = squares a b

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
