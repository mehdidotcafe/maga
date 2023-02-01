{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List (intercalate, map, words, reverse)
import Data.Text (unpack, pack, stripStart, stripEnd)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    putStrLn $ unwords $ map show $ reverse arr