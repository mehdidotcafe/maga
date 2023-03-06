{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

pat = "hackerrank"

hackerrankInString [] p sl pl = "NO"
hackerrankInString s p sl pl
  | sl < pl = "NO"
  | otherwise = case hackerrankInString' s p of
    "YES" -> "YES"
    "NO" -> hackerrankInString (tail s) p (sl - 1) pl
  where
    hackerrankInString' s [] = "YES"
    hackerrankInString' [] p = "NO"
    hackerrankInString' (x:xs) (p:ps)
      | x == p = hackerrankInString' xs ps
      | otherwise = hackerrankInString' xs (p:ps)
    

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

        let result = hackerrankInString s pat (length s) (length pat)

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
