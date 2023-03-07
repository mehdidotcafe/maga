{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Char (isDigit, isLower, isUpper)
import Data.Text ( pack, stripEnd, stripStart, unpack )
import Data.List (findIndex)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )
import Data.Maybe (isNothing)


minLength = 6
specialCharacters = "!@#$%^&*()-+"

lengthValidator :: Int -> Int
lengthValidator n
  | n >= 6 = 0
  | otherwise = minLength - n

digitValidator :: String -> Int
digitValidator x = case findIndex isDigit x of
  Nothing -> 1
  Just e -> 0

lowercaseValidator x = case findIndex isLower x of
  Nothing -> 1
  Just e -> 0

uppercaseValidator x = case findIndex isUpper x of
  Nothing -> 1
  Just e -> 0

isSpecialChar e = e `elem` specialCharacters

specialCharacterValidator x = case findIndex isSpecialChar x of
  Nothing -> 1
  Just e -> 0

minimumNumber n p = max (lengthValidator n) (digitValidator p + lowercaseValidator p + uppercaseValidator p + specialCharacterValidator p)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    password <- getLine

    let answer = minimumNumber n password

    hPutStrLn fptr $ show answer

    hFlush fptr
    hClose fptr
