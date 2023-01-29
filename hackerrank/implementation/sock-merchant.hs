{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (group, map, sort, words)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

sockMerchant = sum . map (flip div 2 . length ) . group . sort
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arTemp <- getLine

    let ar = map (read :: String -> Int) . words $ rstrip arTemp

    let result = sockMerchant ar

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
