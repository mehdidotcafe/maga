{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad ( forM_ )
import Data.Text ( unpack, pack, stripEnd, stripStart, find )
import Data.List (insert, findIndex, delete)
import System.Environment ( getEnv )
import System.IO
    ( hClose, hFlush, hPutStrLn, openFile, IOMode(WriteMode) )

noAnswerMessage = "no answer"

biggerIsGreater [] s = noAnswerMessage
biggerIsGreater (x:xs) s = case findIndex (> x) s of
    Nothing -> biggerIsGreater xs $ insert x s
    Just i -> reverse xs ++ [s!!i] ++ insert x (delete (s!!i) s)

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        w <- getLine

        let result = biggerIsGreater (reverse w) []

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
