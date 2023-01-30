{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.List (maximum, words, map)
import Data.Text (unpack, pack, stripEnd, stripStart)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

aLetterAsciiValue = fromEnum 'a'

designerPdfViewer :: [Int] -> [Char] -> Int
designerPdfViewer h word = wordLength * wordHeight
  where
    wordLength = length word
    wordHeight = maximum $ map (\letter -> h!!(fromEnum letter - aLetterAsciiValue)) word

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    hTemp <- getLine

    let h = map (read :: String -> Int) . words $ rstrip hTemp

    word <- getLine

    let result = designerPdfViewer h word

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
