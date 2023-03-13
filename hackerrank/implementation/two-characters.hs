{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Set (fromList, toList)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

isValid [] = True
isValid [x] = True
isValid [x, y] = x /= y
isValid (x:y:z:xs) = y /= x && y /= z && z == x && isValid (y:z:xs)

remLetter s l l2 = filter (\x -> x == l || x == l2) s

alternate s [l] [] = 0
alternate s (l:ls) [] = alternate s ls (tail ls)
alternate s (l:ls) (l2:l2s)
  | isValid rs = max (length rs) nv
  | otherwise = nv
  where
    nv = alternate s (l:ls) l2s
    rs = remLetter s l l2

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    lTemp <- getLine
    let l = read $ lstrip $ rstrip lTemp :: Int

    s <- getLine

    let letters = toList (fromList s)

    let result = case () of
          _ | length s < 2 -> 0
            | isValid s -> length s
            | otherwise -> alternate s letters (tail letters)

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
