{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.List (filter)
import Data.Text ( pack, stripEnd, stripStart, unpack, intercalate )

errorMessage = "INVALID RANGE"

isKaprekarNumber n =
  let
    r = powN `mod` m
    l = powN `div` m
    m = 10 ^ digits
    digits = ceiling $ (logBase 10 $ fromIntegral n + 1)
    powN = n^2
  in  r + l == n

lstrip = unpack . stripStart . pack
rstrip = unpack . stripEnd . pack

main :: IO()
main = do
    pTemp <- getLine
    let p = read $ lstrip $ rstrip pTemp :: Int

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    putStrLn $ case filter isKaprekarNumber [p..q] of
      [] -> errorMessage
      numbers -> unwords $ map show numbers