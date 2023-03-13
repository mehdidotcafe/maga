{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )


cmp [x] [y] = True
cmp (x1:x2:xs) (y1:y2:ys) = abs (fromEnum x1 - fromEnum x2) == abs (fromEnum y1 - fromEnum y2) && cmp (x2:xs) (y2:ys)

funnyString s = if cmp s rs then "Funny" else "Not Funny"
  where
    rs = reverse s

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \q_itr -> do
        s <- getLine

        let result = funnyString s

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
