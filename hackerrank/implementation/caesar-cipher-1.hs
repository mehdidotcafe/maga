{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

asciiLowerMod = 97
asciiUpperMod = 65
charsLength = length ['a' .. 'z'] 

caesarCipher [] k = ""
caesarCipher (x:xs) k = cx : caesarCipher xs k
    where
      cx    | isAsciiLower x = toEnum ((x' - asciiLowerMod + k) `mod` charsLength + asciiLowerMod)
            | isAsciiUpper x = toEnum ((x' - asciiUpperMod + k) `mod` charsLength + asciiUpperMod)
            | otherwise = x
      x' = fromEnum x

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    s <- getLine

    kTemp <- getLine
    let k = read $ lstrip $ rstrip kTemp :: Int

    let result = caesarCipher s k

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
