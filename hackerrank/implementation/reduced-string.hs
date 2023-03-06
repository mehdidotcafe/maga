{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

superReducedString s = case superReducedString' s of
  v | v == s -> v
    | otherwise -> superReducedString v
  where
    superReducedString' [] = []
    superReducedString' [y] = [y]
    superReducedString' (x:y:xs) 
      | x == y = superReducedString' xs
      | otherwise = x : superReducedString' (y:xs)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = case superReducedString s of
          v | v == "" -> "Empty String"
            | otherwise -> v

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
