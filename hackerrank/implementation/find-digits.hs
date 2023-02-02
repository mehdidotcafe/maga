module Main where

import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, openFile, hPrint, IOMode(WriteMode) )

findDigits n 0 = 0
findDigits n n'
  | m /= 0 && n `mod` m == 0 = 1 + next
  | otherwise = next
  where
    m = n' `mod` 10
    next = findDigits n (n' `div` 10)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        nTemp <- getLine
        let n = read $ lstrip $ rstrip nTemp :: Int

        hPrint fptr (findDigits n n)

    hFlush fptr
    hClose fptr
